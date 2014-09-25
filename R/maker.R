##' The actual maker object to interact with.
##' @title Main maker object
##' @export
##' @importFrom R6 R6Class
maker <- R6Class(
  "maker",
  public=list(
    file=NULL,
    path=NULL,
    store=NULL,
    sources=NULL,
    packages=NULL,
    targets=NULL,
    verbose=NULL,
    default=NULL,

    initialize=function(maker_file="maker.yml", path=".", verbose=TRUE) {
      self$file <- maker_file
      self$path <- path
      self$verbose <- verbose
      self$reload()
    },

    reload=function() {
      self$store <- store$new(self$path)
      config <- read_maker_file(self$file)
      self$sources <- config$sources
      self$packages <- config$packages
      self$targets <- config$targets
      self$default <- config$target_default
      private$initialize_cleanup_targets()
      for (t in self$targets) {
        t$activate(self)
      }
      self$store$env <- managed_environment$new(self$packages, self$sources)
      self$store$deps <- code_deps$new(self$store$env$env)
    },

    make=function(target_name=NULL, dry_run=FALSE) {
      if (is.null(target_name)) {
        target_name <- self$target_default()
      }
      reloaded <- self$store$env$reload()
      if (reloaded) {
        self$print_message("READ", "", "# reloading sources")
      }
      graph <- self$dependency_graph()
      plan <- dependencies(target_name, graph)
      for (i in plan) {
        self$update(i, dry_run)
      }
    },

    update=function(target_name, dry_run=FALSE) {
      target <- self$get_target(target_name)
      current <- target$is_current()
      if (self$verbose) {
        status <- target$status_string(current)
        cmd <- target$run_fake()
        self$print_message(status, target_name, cmd)
      }
      if (!current && !dry_run) {
        target$build()
      }
    },

    print_message=function(status, target_name, cmd, round=FALSE) {
      fmt <- if (round) "( %5s ) %s" else "[ %5s ] %s"
      str <- sprintf(fmt, status, target_name)
      if (!is.null(cmd)) {
        width <- getOption("width")
        w1 <- max(nchar(self$target_names())) + 10
        w2 <- ceiling(width / 2)
        w <- max(0, min(w1, w2) - nchar(str))
        pos <- width - (nchar(str) + w)
        join <- " |  "
        cmd <- abbreviate(cmd, pos - nchar(join))
        if (length(cmd) == 1) {
          pad <- paste(rep(" ", w), collapse="")
          str <- paste0(str, pad, join, cmd)
        }
      }
      message(str)
    },

    remove_targets=function(target_names) {
      for (t in target_names) {
        self$remove_target(t)
      }
    },

    remove_target=function(target_name) {
      target <- self$get_target(target_name)
      did_remove <- target$del(missing_ok=TRUE)
      if (self$verbose) {
        if (did_remove) {
          status <- "DEL"
          fn <- if (target$type == "object") "rm" else "file.remove"
          cmd <- sprintf('%s("%s")', fn, target_name)
        } else {
          status <- ""
          cmd <- NULL
        }
        self$print_message(status, target_name, cmd, TRUE)
      }
    },

    get_target=function(target_name) {
      if (!(target_name %in% self$target_names())) {
        stop("No such target ", target_name)
      }
      self$targets[[target_name]]
    },

    get_targets=function(target_names) {
      if (!all(target_names %in% self$target_names())) {
        stop("No such target ",
             paste(setdiff(target_names, self$target_names()), collapse=", "))
      }
      self$targets[target_names]
    },

    add_targets=function(x, force=FALSE) {
      if (!all(sapply(x, inherits, "target"))) {
        stop("All elements must be targets")
      }
      target_names <- vapply(x, "[[", character(1), "name")
      if (any(duplicated(target_names))) {
        stop("All target names must be unique")
      }
      if (any(target_names %in% self$target_names())) {
        if (force) {
          to_drop <- self$target_names() %in% target_names
          if (any(to_drop)) {
            self$targets <- self$targets[!to_drop]
          }
        } else {
          stop("Targets already present: ",
               paste(intersect(target_names, self$target_names()),
                     collapse=", "))
        }
      }
      names(x) <- target_names
      self$targets <- c(self$targets, x)
    },

    target_names=function() {
      names(self$targets)
    },

    target_default=function() {
      if (is.null(self$default)) {
        stop(self$file,
             " does not define 'target_default' or have target 'all'")
      }
      self$default
    },

    ## Wrappers around the *object* store.  Not sure about the other
    ## stores, really.
    ls=function() {
      self$store$objects$ls()
    },
    get=function(name) {
      self$store$objects$get(name)
    },
    export=function(names, envir=.GlobalEnv) {
      self$store$objects$export(names, envir)
    },

    ## Things that just pass through to the targets:
    is_current=function(target_name) {
      self$get_target(target_name)$is_current()
    },
    dependency_status=function(target_name, missing_ok=FALSE) {
      self$get_target(target_name)$dependency_status(missing_ok)
    },
    build=function(target_name) {
      self$get_target(target_name)$build()
    },

    dependency_graph=function() {
      targets <- self$target_names()
      g <- lapply(targets, function(t) self$get_target(t)$dependencies())
      names(g) <- targets
      topological_sort(g)
    },

    diagram=function() {
      diagram(self)
    }
    ),
  private=list(
    initialize_cleanup_targets=function() {
      levels <- cleanup_target_names()
      targets <- list()
      for (i in seq_along(levels)) {
        target_name <- levels[[i]]
        if (target_name %in% self$target_names()) {
          depends <- self$get_target(target_name)$depends
          rule    <- self$get_target(target_name)$rule
        } else {
          depends <- rule <- NULL
        }
        if (i > 1L) {
          depends <- c(depends, list(levels[[i - 1L]]))
        }
        targets[[i]] <- make_target(target_name,
                                    list(rule=rule, depends=depends),
                                    "cleanup")
      }
      self$add_targets(targets, force=TRUE)
    }
    ))

read_maker_file <- function(filename) {
  dat <- yaml_read(filename)

  warn_unknown(filename, dat, c("packages", "sources",
                                "target_default", "targets"))

  dat$packages <- with_default(dat$packages, character(0))
  dat$sources  <- with_default(dat$sources,  character(0))

  assert_character(dat$packages, "packages")
  assert_character(dat$sources,  "sources")
  if (!all(file.exists(dat$sources))) {
    stop("All files in 'sources' must exist")
  }
  if (any(duplicated(names(dat$targets)))) {
    stop("All target names must be unique")
  }

  dat$targets <- lnapply(dat$targets, make_target)

  if (is.null(dat$target_default) && "all" %in% names(dat$targets)) {
    dat$target_default <- "all"
  }
  if (!is.null(dat$target_default)) {
    assert_scalar_character(dat$target_default, "target_default")
  }
  dat
}

cleanup_levels <- function() {
  c("tidy", "clean", "purge", "never")
}

cleanup_target_names <- function() {
  c("tidy", "clean", "purge")
}

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
      self$targets <- NULL
      self$add_targets(config$targets)
      private$initialize_cleanup_targets()
      private$initialize_targets_activate()
      private$initialize_default_target(config$target_default)
      private$initialize_utility_targets() # last; nothing depends on these
      private$initialize_message_format()
      self$store$env <- managed_environment$new(self$packages, self$sources)
    },

    make=function(target_name=NULL, dry_run=FALSE, force=FALSE,
      force_all=FALSE) {
      if (is.null(target_name)) {
        target_name <- self$target_default()
      }
      ## NOTE: Not 100% sure about this.  The "deps" target requires
      ## that the sources are not loaded before it is run, because it
      ## exists to install required packages.  So it needs to be
      ## picked up here specially.
      if (target_name != "deps") {
        self$load_sources(show_message=!is.null(self$store$env$env))
      }
      graph <- self$dependency_graph()
      plan <- dependencies(target_name, graph)
      for (i in plan) {
        self$update(i, dry_run, force_all || (force && i == target_name))
      }
    },

    load_sources=function(show_message=TRUE) {
      force(show_message) # stupid delayed evaluation
      first <- is.null(self$store$env$env)
      reloaded <- self$store$env$reload()
      if (show_message && reloaded) {
        cmd <- sprintf("# %s sources",
                       if (first) "loading" else "reloading")
        self$print_message("READ", "", cmd)
      }
    },

    update=function(target_name, dry_run=FALSE, force=FALSE) {
      target <- self$get_target(target_name)
      current <- !force && target$is_current()
      if (self$verbose) {
        status <- target$status_string(current)
        cmd <- if (current) NULL else target$run_fake()
        self$print_message(status, target_name, cmd)
      }
      if (!current && !dry_run) {
        target$build()
      }
    },

    print_message=function(status, target_name, cmd=NULL, style="square") {
      paint <- private$fmt$p$paint
      col <- status_colour(status)
      status <- brackets(paint(sprintf("%5s", status), col), style)
      if (!is.null(cmd)) {
        w_extra <- max(0, nchar(target_name) - private$fmt$target_width)
        cmd <- abbreviate(cmd, private$fmt$max_cmd_width - w_extra)
      }
      if (is.null(cmd)) {
        str <- sprintf(private$fmt$no_cmd, status, target_name)
      } else {
        str <- sprintf(private$fmt$with_cmd, status, target_name,
                       private$fmt$p$paint(cmd, "grey"))
      }
      message(str)
    },

    expire=function(target_name, recursive=FALSE) {
      if (recursive) {
        graph <- self$dependency_graph()
        for (t in dependencies(target_name, graph)) {
          self$expire(t)
        }
      } else {
        self$store$db$del(target_name, missing_ok=TRUE)
      }
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
        self$print_message(status, target_name, cmd, "round")
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

    get_targets_by_type=function(types) {
      filter_targets_by_type(self$targets, types)
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
    fmt=NULL,

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
    },

    initialize_utility_targets=function() {
      add <- list(target_utility$new("deps", utility_deps, self),
                  target_utility$new("gitignore", utility_gitignore, self))
      self$add_targets(add)
    },

    ## NOTE: The logic here seems remarkably clumsy.
    initialize_default_target=function(default) {
      if (is.null(default)) {
        if ("all" %in% self$target_names()) {
          self$default <- "all"
        }
      } else {
        assert_scalar_character(default, "target_default")
        if (default %in% self$target_names()) {
          stop(sprintf("Default target %s not found in makerfile"),
               default)
        }
        self$default <- default
      }
    },

    initialize_targets_activate=function() {
      for (t in self$targets) {
        t$activate(self)
      }
    },

    initialize_message_format=function() {
      width <- getOption("width")
      w0 <- 10 # nchar("[ BUILD ] ")
      target_width <- min(max(nchar(self$target_names())) + w0,
                      ceiling(width / 2))
      private$fmt <- list(
        no_cmd="%s %s",
        with_cmd=sprintf("%%s %%-%ds |  %%s", target_width),
        target_width=target_width,
        max_cmd_width=width - (w0 + 1 + target_width + 4),
        p=painter$new(interactive()))
    }
    ))

read_maker_file <- function(filename) {
  dat <- yaml_read(filename)
  warn_unknown(filename, dat,
               c("packages", "sources", "target_default", "targets"))
  dat$packages <- with_default(dat$packages, character(0))
  dat$sources  <- with_default(dat$sources,  character(0))
  dat$targets <- lnapply(dat$targets, make_target)
  dat
}

cleanup_levels <- function() {
  c("tidy", "clean", "purge", "never")
}

cleanup_target_names <- function() {
  c("tidy", "clean", "purge")
}

## Not sure I have a full list of these yet:
status_colour <- function(str) {
  switch(str,
         BUILD="steelblue4",
         OK="green3",
         CLEAN="orange",
         DEL="red",
         UTIL="darkorchid3",
         READ="yellow",
         NULL)
}

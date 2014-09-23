## TODO: Merge this into documentation.
## * `packages`: vector of packages to read
## * `sources`: vector of source files to read
## * `default_target`: name of the default target (within `targets`)
## * `targets`: an unordered dictionary of targets to build.  Each
##   target has form:
##
## name:
##   rule: generating_function
##   depends: <see below>
##
## The name is a character string and must be unique within this file.
## If it contains a slash (/) or ends in one of the filename
## extensions it will be assumed to be a file.  Otherwise it is
## assumed to be the identifier for an R object.

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
    env=NULL,

    initialize=function(maker_file="maker.yml", path=".") {
      self$file <- maker_file
      self$path <- path
      self$reload()
    },

    reload=function() {
      self$store <- store$new(self$path)
      config <- read_maker_file(self$file)
      self$sources <- config$sources
      self$packages <- config$packages
      self$targets <- config$targets
      private$initialize_cleanup_targets()
      for (t in self$targets) {
        t$activate(self)
      }
      self$env <- create_environment(sources=self$sources,
                                     packages=self$packages)
      self$store$deps <- code_deps$new(self$env)
    },

    ## This *computes the current status*, which can be fetched later
    ## with:
    ##   self$store$db$get(target_name)
    dependency_status=function(target_name, missing_ok=FALSE) {
      self$get_target(target_name)$dependency_status(missing_ok)
    },

    build=function(target_name) {
      self$get_target(target_name)$build()
    },

    ## Really, when doing a dry_run, the status of a target is current
    ## if UNKNOWN if a dependency has status BUILD.  For now, I'm not
    ## worrying about that though.
    update=function(target_name, verbose=TRUE, dry_run=FALSE,
      step=NULL, nsteps=NULL) {
      current <- self$is_current(target_name)
      if (verbose) {
        self$print_message(target_name, current, step, nsteps)
      }
      if (!current && !dry_run) {
        self$build(target_name)
      }
    },

    print_message=function(target_name, current, step, nsteps) {
      target <- self$get_target(target_name)
      status <- target$status_string(current)
      message(sprintf("[ %5s ] %s", status, target_name))
    },

    make=function(target_name=NULL, verbose=TRUE, dry_run=FALSE) {
      if (is.null(target_name)) {
        target_name <- self$target_default()
      }
      graph <- self$dependency_graph()
      plan <- dependencies(target_name, graph)
      len <- length(plan)
      for (i in seq_len(len)) {
        self$update(plan[[i]], verbose, dry_run, i, len)
      }
    },

    cleanup=function(level="tidy", verbose=TRUE) {
      levels <- cleanup_levels()
      level <- match_value(level, setdiff(levels, "never"))
      targets <- self$get_targets(self$target_names())
      target_level <- sapply(targets, function(x) x$cleanup_level)
      self$remove_targets(names(targets)[target_level == level], verbose)
    },

    remove_targets=function(target_names, verbose=TRUE) {
      for (t in target_names) {
        self$remove_target(t, verbose)
      }
    },

    remove_target=function(target_name, verbose=TRUE) {
      did_remove <- self$get_target(target_name)$del(missing_ok=TRUE)
      if (verbose) {
        status <- if (did_remove) "DEL" else ""
        message(sprintf("[ %5s ] %s", status, target_name))
      }
    },

    is_current=function(target_name) {
      self$get_target(target_name)$is_current()
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
          private$drop_targets(intersect(target_names, self$target_names()))
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
      names(self$targets[1])
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
        targets[[i]] <- target$new(target_name, "cleanup", rule, depends)
      }
      self$add_targets(targets, force=TRUE)
    },

    drop_targets=function(x) {
      self$targets <- self$targets[!(names(self$targets) %in% x)]
    }
    ))

read_maker_file <- function(filename) {
  dat <- yaml_read(filename)

  warn_unknown(filename, dat, c("packages", "sources", "targets"))

  dat$packages <- with_default(dat$packages, character(0))
  dat$sources  <- with_default(dat$sources,  character(0))

  if (!is.character(dat$packages)) {
    stop("'packages' must be a character vector")
  }
  if (!is.character(dat$sources)) {
    stop("'sources' must be a character vector")
  }
  if (!all(file.exists(dat$sources))) {
    stop("All files in 'sources' must exist")
  }
  if (any(duplicated(names(dat$targets)))) {
    stop("All target names must be unique")
  }

  validate_target <- function(target_name, obj) {
    warn_unknown(target_name, obj,
                 c("rule", "depends", "target_argument_name",
                   "cleanup_level",
                   # Special things
                   "plot"))
    type <- target_type(target_name)
    if (type == "object" && is.null(obj$rule)) {
      type <- "fake"
    }
    t <- target$new(target_name, type, obj$rule, depends=obj$depends,
                    cleanup=with_default(obj$cleanup_level, "tidy"),
                    target_argument_name=obj$target_argument_name)

    ## NOTE: Undecided whether to put this into the target
    ## initialize() method or not.  For now I'll just manually hammer
    ## it in here.  Possibly better (though this really requires the
    ## self-building targets to make much sense) is to make a new
    ## class that inherits target that will take care of this stuff.
    ## I think that's a better call.
    if ("plot" %in% names(obj)) {
      if (identical(obj$plot, TRUE) || is.null(obj$plot)) {
        obj$plot <- list()
      }
      assert_list(obj$plot)
      assert_named(obj$plot)
      dev <- get_device(tools::file_ext(target_name))
      ## This will not work well for cases where `...` is in the
      ## device name (such as jpeg, bmp, etc)
      warn_unknown(paste0(target_name, ":plot"), obj$plot,
                   names(formals(dev)))
      t$plot <- list(device=dev, args=obj$plot)
    }
    t
  }
  dat$targets <- lnapply(dat$targets, validate_target)
  dat
}

cleanup_levels <- function() {
  c("tidy", "clean", "purge", "never")
}

cleanup_target_names <- function() {
  c("tidy", "clean", "purge")
}

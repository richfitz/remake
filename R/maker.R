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
    config=NULL,
    env=NULL,

    initialize=function(maker_file="maker.yml", path=".") {
      self$file <- maker_file
      self$path <- path
      self$reload()
    },

    reload=function() {
      self$store <- store$new(self$path)
      self$config <- read_maker_file(self$file)
      for (t in self$config$targets) {
        t$initialise_depends(self)
      }
      self$build_environment()
    },

    build_environment=function() {
      self$env <- create_environment(sources=self$config$sources,
                                     packages=self$config$packages)
    },

    ## This *computes the current status*, which can be fetched later
    ## with:
    ##   self$store$db$get(target_name)
    dependency_status=function(target_name, missing_ok=FALSE) {
      dependency_status(self$get_target(target_name), self$store,
                        missing_ok=missing_ok)
    },

    build=function(target_name) {
      target <- self$get_target(target_name)
      if (target$implicit) {
        stop("Can't build implicit targets")
      }
      if (is.null(target$rule)) {
        return()
      }
      ## This avoids either manually creating directories, or obscure
      ## errors when R can't save a file to a place.  Possibly this
      ## should be a configurable behaviour, but we're guaranteed to
      ## be working with genuine files so this should be harmless.
      if (target$type == "file") {
        dir.create(dirname(target$name), showWarnings=FALSE, recursive=TRUE)
      }
      res <- do_run(self$get_target(target_name), self$store, self$env)
      if (target$type == "object") {
        self$store$objects$set(target_name, res)
      }
      self$store$db$set(target_name, self$dependency_status(target_name))
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
      status <- self$status_string(target_name, current)
      if (is.null(step)) {
        msg <- sprintf("[ %5s ] %s", status, target_name)
      } else {
        w <- nchar(nsteps) - 1L
        msg <- sprintf("(%s / %s) [ %5s ] %s",
                       formatC(step, w), formatC(nsteps, w),
                       status, target_name)
      }
      message(msg)
    },

    status_string=function(target_name, current=NULL) {
      if (is.null(current)) {
        current <- self$is_current(target_name)
      }
      if (is.null(self$get_target(target_name)$rule)) {
        ""
      } else if (current) {
        "OK"
      } else {
        "BUILD"
      }
    },

    make=function(target_name, verbose=TRUE, dry_run=FALSE) {
      graph <- self$dependency_graph()
      plan <- dependencies(target_name, graph)
      len <- length(plan)
      for (i in seq_len(len)) {
        self$update(plan[[i]], verbose, dry_run, i, len)
      }
    },

    is_current=function(target_name) {
      is_current(self$get_target(target_name), self$store)
    },

    get_target=function(target_name) {
      if (!(target_name %in% self$target_names())) {
        stop("No such target ", target_name)
      }
      self$config$targets[[target_name]]
    },

    get_targets=function(target_names) {
      if (!all(target_names %in% self$target_names())) {
        stop("No such target ",
             paste(setdiff(target_names, self$target_names()), collapse=", "))
      }
      self$config$targets[target_names]
    },

    add_targets=function(x) {
      if (!all(sapply(x, inherits, "target"))) {
        stop("All elements must be targets")
      }
      target_names <- vapply(x, "[[", character(1), "name")
      if (any(duplicated(target_names))) {
        stop("All target names must be unique")
      }
      if (any(target_names %in% self$target_names())) {
        stop("Targets already present: ",
             paste(intersect(target_names, self$target_names()),
                   collapse=", "))
      }
      names(x) <- target_names
      self$config$targets <- c(self$config$targets, x)
    },

    target_names=function() {
      names(self$config$targets)
    },

    dependency_graph=function() {
      targets <- self$target_names()
      g <- lapply(targets, function(t) self$get_target(t)$dependencies())
      names(g) <- targets
      topological_sort(g)
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
    warn_unknown(target_name, target,
                 c("rule", "depends", "target_argument_name"))
    target$new(target_name, obj$rule, obj$depends, obj$target_argument_name)
  }
  dat$targets <- lnapply(dat$targets, validate_target)
  dat
}

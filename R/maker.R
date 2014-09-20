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
      self$config <- read_maker(self$file)
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
      ## TODO: Probably shortcut return here on NULL rules?
      if (target$implicit) {
        stop("Can't build implicit targets")
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
    update=function(target_name, verbose=TRUE, dry_run=FALSE) {
      current <- self$is_current(target_name)
      if (verbose) {
        status <- if (current) "OK" else "BUILD"
        self$print_message(target_name, status)
      }
      if (!current && !dry_run) {
        self$build(target_name)
      }
    },

    make=function(target_name, verbose=TRUE, dry_run=FALSE) {
      graph <- self$dependency_graph()
      plan <- dependencies(target_name, graph)
      for (target in plan) {
        self$update(target, verbose, dry_run)
      }
    },

    print_message=function(target_name, status) {
      ## 'status' is one of BUILD / OK
      message(sprintf("[ %5s ] %s", status, target_name))
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

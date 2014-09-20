## This one has the potential issue that if the hash and the file get
## out of sync we have problems.  However, this is controlled within
## the .maker/objects directory so we can kind of assume that is not
## going to happen.
##
## Storing hashes as <name>__hash means that an object with that
## pattern really can't be stored.
##
## We also can't store objects that have names that aren't filename
## OK.  Most of those also aren't R OK, but I should decide if we'll
## hash object names coming in.  A good idea could be to hash the
## name, then have
##   <digest>       -- actual data
##   <digest>__hash -- hash of the data
##   <digest>__name -- name of the data
object_store <- R6Class(
  "object_store",
  public=list(
    path=NULL,
    initialize=function(path) {
      self$path <- path
      dir.create(self$path, FALSE)
    },

    contains=function(key) {
      file.exists(self$fullname(key))
    },

    get=function(key) {
      readRDS(self$fullname(key))
    },

    set=function(key, value) {
      hash <- digest::digest(value)
      saveRDS(value, self$fullname(key))
      writeLines(hash, self$hashname(key))
    },

    del=function(key) {
      file.remove(self$fullname(key))
    },

    get_hash=function(key, missing_ok=FALSE) {
      file <- self$hashname(key)
      exists <- file.exists(file)
      if (exists) {
        readLines(file)
      } else if (missing_ok) {
        NA_character_
      } else {
        stop(sprintf("key %s not found in object store", key))
      }
    },

    ## This is not really used that often.
    ls=function() {
      grep("__hash$", dir(self$path), invert=TRUE, value=TRUE)
    },

    ## This is a *one way* function; changes won't be automatically
    ## propagated.
    export=function(list=NULL, envir=.GlobalEnv) {
      if (is.null(list)) {
        list <- self$ls()
      }
      for (i in list) {
        assign(i, self$get(i), envir=envir)
      }
    },

    ## This could be the inverse of export(), perhaps.
    ## import=function(list=NULL, envir=.GlobalEnv) {
    ##   if (is.null(list)) {
    ##     list <- intersect(self$ls(), ls(envir))
    ##   }
    ##   for (i in list) {
    ##     self$set(i, get(i, envir))
    ##   }
    ## },

    fullname=function(key) {
      file.path(self$path, key)
    },

    hashname=function(key) {
      paste0(self$fullname(key), "__hash")
    }
    ))

## This one is quite different.  It also does not try to do anything
## clever with caching this information.  The files on disk are
## allowed to change independently of R.
##
## The hashes are computed *every time* these files are touched.
## That's unlikely to be nice for very large files (md5 is not that
## fast).  Might be that we can do better by comparing size or
## modification time first to detect changes early, but to detect that
## things are the same I don't think there's a choice but to rehash.
## For very large objects this is going to be very slow (I see 2.5s
## for a 650MB file).  But then *doing* anything with objects that
## size is going to be entertaining anyway.
##
## An alternative is moving to:
##   https://code.google.com/p/xxhash/
## But before going that route, need to check that we're not simply
## limited by disk speed.  Alsp:
##   http://cran.r-project.org/web/packages/hashFunction/
##
## All hell is going to break loose if people use setwd() with this.
file_store <- R6Class(
  "file_store",
  public=list(
    contains=function(filename) {
      file.exists(self$fullname(filename))
    },

    get_hash=function(filename, missing_ok=FALSE) {
      file <- self$fullname(filename)
      exists <- file.exists(file)
      if (exists) {
        unname(tools::md5sum(self$fullname(filename)))
      } else if (missing_ok) {
        NA_character_
      } else {
        stop(sprintf("file %s not found in file store", filename))
      }
    },

    fullname=function(filename) {
      filename
    },

    ## Deal with directories here?  Probably not.
    del=function(filename) {
      file.remove(self$fullname(filename))
    }
    ))

## This one holds the database information.
##' @importFrom digest digest
##' @importFrom rjson toJSON fromJSON
maker_db <- R6Class(
  "maker_db",
  public=list(
    path=NULL,

    initialize=function(path) {
      self$path <- path
      dir.create(self$path, FALSE)
    },

    get=function(key) {
      fromJSON(file=self$fullname(key))
    },

    ## TODO: Possibly useful to check for a 'name' key here or we'll
    ## corrupt the database.
    set=function(key, value) {
      str <- toJSON(value)
      writeLines(str, self$fullname(key))
    },

    del=function(key) {
      file.remove(self$fullname(key))
    },

    contains=function(key) {
      file.exists(self$fullname(key))
    },

    ls=function() {
      files <- dir(self$path, pattern="\\.json$", full.names=TRUE)
      sapply(files, function(x) fromJSON(x)$name)
    },

    ## We hash keys here so that things like file paths (with slashes,
    ## etc) are OK to use.  It's not super important to hash the
    ## *objects*, but at the same time the less we care about the
    ## better.  People should never directly interact with the files
    ## in the directory, so it's OK.
    fullname=function(key) {
      file.path(self$path, paste0(digest(key), ".json"))
    }
    ))

##' The data store used by maker.  This is an R6 class, and generally
##' does not need creating manually.  It's basically a pair of
##' key-value stores; one for R objects and one for files.
##' @title Data store used by maker
##' @export
##' @importFrom R6 R6Class
store <- R6Class(
  "store",
  public=list(
    db=NULL,
    objects=NULL,
    files=NULL,
    path=NULL,

    initialize=function(path=".") {
      dir.create(path, FALSE, TRUE)
      self$path    <- file.path(normalizePath(path, mustWork=TRUE), ".maker")
      dir.create(self$path, FALSE, TRUE)
      self$db <- maker_db$new(file.path(self$path, "db"))
      self$objects <- object_store$new(file.path(self$path, "objects"))
      self$files <- file_store$new()
    },

    destroy=function() {
      unlink(self$path, recursive=TRUE)
      self$path <- NULL
      self$db <- NULL
      self$objects <- NULL
      self$files <- NULL
    },

    contains=function(name, type=NULL) {
      private$right_store(type)$contains(name)
    },

    get_hash=function(name, type=NULL, missing_ok) {
      if (missing_ok && !self$contains(name, type)) {
        NA_character_
      } else {
        private$right_store(type)$get_hash(name)
      }
    }),
  private=list(
    right_store=function(type) {
      if (is.null(type)) {
        stop("type must be specified")
      }
      type <- match.arg(type, c("file", "object"))
      switch(type,
             file=self$files,
             object=self$objects,
             stop("Invalid type ", dQuote(type)))
    }))


##' The actual maker object to interact with.
##' @title Main maker object
##' @export
##' @importFrom R6 R6Class
maker <- R6Class(
  "maker",
  public=list(
    store=NULL,
    config=NULL,
    env=NULL,

    initialize=function(maker_file="maker.yml", path=".") {
      self$store <- store$new(path)
      self$config <- read_maker(maker_file)
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

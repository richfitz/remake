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
    exists=function(filename) {
      file.exists(self$fullname(filename))
    },

    ## Deal with directories here?  Probably not.
    del=function(filename, missing_ok=FALSE) {
      ## TODO: Generalise this pattern (see also get_hash)
      exists <- self$exists(filename)
      if (exists) {
        file_remove(self$fullname(filename))
      } else if (!missing_ok) {
        stop(sprintf("file %s not found in file store", filename))
      }
      invisible(exists)
    },

    archive_export=function(filename, path, missing_ok=FALSE) {
      dir.create(path, FALSE, TRUE)
      assert_directory(path)
      exists <- self$exists(filename)
      if (exists) {
        full <- self$fullname(filename)
        dest <- file.path(path, dirname(full))
        dir.create(dest, FALSE, TRUE)
        file_copy(full, dest, warn=!missing_ok)
      } else if (!missing_ok) {
        stop(sprintf("file %s not found in file store", filename))
      }
      invisible(exists)
    },

    archive_import=function(filename, path) {
      assert_directory(path)
      file_in <- file.path(path, filename)
      assert_file_exists(file_in)
      path_out <- dirname(filename)
      dir.create(path_out, showWarnings=FALSE, recursive=TRUE)
      file_copy(file_in, path_out)
    },

    get_hash=function(filename, missing_ok=FALSE) {
      exists <- self$exists(filename)
      if (exists) {
        hash_files(filename, named=FALSE)
      } else if (missing_ok) {
        NA_character_
      } else {
        stop(sprintf("file %s not found in file store", filename))
      }
    },

    fullname=function(filename) {
      filename
    }
    ))

##' @importFrom R6 R6Class
##' @importFrom storr storr_rds
store <- R6Class(
  "store",
  public=list(
    db=NULL,
    objects=NULL,
    files=NULL,
    ## Becomes `sources` I think.
    env=NULL,
    packages=NULL,
    packages_loaded=FALSE,
    path=NULL,
    version=NULL,

    initialize=function(path=".", packages=character(0), sources=character(0)) {
      dir.create(path, FALSE, TRUE)
      self$path    <- file.path(normalizePath(path, mustWork=TRUE), ".remake")
      dir.create(self$path, FALSE, TRUE)
      self$db <- storr_rds(file.path(self$path, "objects"),
                           default_namespace="remake_db",
                           mangle_key=TRUE)
      self$objects <- storr_rds(file.path(self$path, "objects"))
      self$files <- file_store$new()
      self$version <- packageVersion(.packageName)
      self$packages <- packages
      self$env <- managed_environment$new(sources)
    },

    exists=function(name, type) {
      private$right_store(type)$exists(name)
    },

    get_hash=function(name, type, missing_ok) {
      if (missing_ok && !self$exists(name, type)) {
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

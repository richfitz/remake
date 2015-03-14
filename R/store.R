## This one has the potential issue that if the hash and the file get
## out of sync we have problems.  However, this is controlled within
## the .remake/objects directory so we can kind of assume that is not
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
##' @importFrom R6 R6Class
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
      exists <- self$contains(key)
      if (!exists) {
        stop(sprintf("key %s not found in object store", key))
      }
      path <- self$fullname(key)
      readRDS(self$fullname(key))
    },

    set=function(key, value) {
      hash <- self$hash(value)
      saveRDS(value, self$fullname(key))
      writeLines(hash, self$hashname(key))
    },

    del=function(key, missing_ok=FALSE) {
      exists <- self$contains(key)
      file_remove(self$fullname(key), recursive=TRUE)
      file_remove(self$hashname(key))
      if (!exists && !missing_ok) {
        stop(sprintf("key %s not found in object store", key))
      }
      invisible(exists)
    },

    hash=function(value) {
      hash_object(value)
    },

    archive_export=function(key, path, missing_ok=FALSE) {
      dir.create(path, FALSE, TRUE)
      assert_directory(path)
      exists <- self$contains(key)
      if (!exists && !missing_ok) {
        stop(sprintf("key %s not found in object store", key))
      }
      file_copy(self$fullname(key), path, warn=!missing_ok)
      file_copy(self$hashname(key), path, warn=!missing_ok)
      invisible(exists)
    },

    archive_import=function(key, path) {
      assert_directory(path)
      file_in <- file.path(path, key)
      hash_in <- paste0(file_in, "__hash") # TODO: avoid
      assert_file_exists(file_in)
      assert_file_exists(hash_in)
      dir.create(self$path, FALSE) # should not be needed?
      file_copy(file_in, self$path)
      file_copy(hash_in, self$path)
    },

    get_hash=function(key, missing_ok=FALSE) {
      exists <- self$contains(key)
      if (exists) {
        readLines(self$hashname(key))
      } else if (missing_ok) {
        NA_character_
      } else {
        stop(sprintf("key %s not found in object store", key))
      }
    },

    ## This is not really used that often.
    ls=function() {
      c(grep("__hash$", dir(self$path), invert=TRUE, value=TRUE),
        basename(list.dirs(self$path, recursive=FALSE)))
    },

    ## This is a *one way* function; changes won't be automatically
    ## propagated.
    ##
    ## TODO: need to assert that everything is here before running
    ## export, otherwise we get weird errors.
    export=function(list=NULL, envir=.GlobalEnv, delayed=FALSE) {
      if (is.null(list)) {
        list <- self$ls()
      }
      assert_character(list)

      if (is.null(names(list))) {
        names_out <- list
      } else {
        names_out <- names(list)
        names_out[names_out == ""] <- list[names_out == ""]
      }

      do_assign <- function(name_out, name_in) {
        if (delayed) {
          force(name_out)
          force(name_in)
          delayedAssign(name_out, self$get(name_in), assign.env=envir)
        } else {
          assign(name_out, self$get(name_in), envir=envir)
        }
      }
      ## msg <- !self$contains(list)
      ## if (any(msg)) {
      ##   stop("Missing objects: ", paste(msg, collapse=", "))
      ## }
      for (i in seq_along(list)) {
        do_assign(names_out[i], list[i])
      }
    },

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

    ## Deal with directories here?  Probably not.
    del=function(filename, missing_ok=FALSE) {
      ## TODO: Generalise this pattern (see also get_hash)
      exists <- self$contains(filename)
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
      exists <- self$contains(filename)
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
      exists <- self$contains(filename)
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

## This one holds the database information.
remake_db <- R6Class(
  "remake_db",
  public=list(
    path=NULL,

    initialize=function(path) {
      self$path <- path
      dir.create(self$path, FALSE)
    },

    get=function(key) {
      readRDS(file=self$fullname(key))
    },

    ## TODO: Possibly useful to check for a 'name' key here or we'll
    ## corrupt the database.
    set=function(key, value) {
      saveRDS(value, self$fullname(key))
    },

    del=function(key, missing_ok=FALSE) {
      exists <- self$contains(key)
      if (exists) {
        file_remove(self$fullname(key))
      } else if (!missing_ok) {
        stop(sprintf("key %s not found in remake database", key))
      }
      invisible(exists)
    },

    archive_export=function(key, path, missing_ok=FALSE) {
      dir.create(path, FALSE, TRUE)
      assert_directory(path)
      exists <- self$contains(key)
      if (exists) {
        file_copy(self$fullname(key), path, warn=!missing_ok)
      } else if (!missing_ok) {
        stop(sprintf("key %s not found in remake database", key))
      }
      invisible(exists)
    },

    archive_import=function(key, path) {
      assert_directory(path)
      file_in <- file.path(path, self$rdsname(key))
      assert_file_exists(file_in)
      ## This should never fail:
      if (readRDS(file_in)$name != key) {
        stop("Corrupt database detected")
      }
      file_copy(file_in, self$path)
    },

    contains=function(key) {
      file.exists(self$fullname(key))
    },

    ls=function() {
      files <- dir(self$path, pattern="\\.rds$", full.names=TRUE)
      vcapply(files, function(x) readRDS(x)$name)
    },

    ## We hash keys here so that things like file paths (with slashes,
    ## etc) are OK to use.  It's not super important to hash the
    ## *objects*, but at the same time the less we care about the
    ## better.  People should never directly interact with the files
    ## in the directory, so it's OK.
    fullname=function(key) {
      file.path(self$path, self$rdsname(key))
    },

    rdsname=function(key) {
      paste0(hash_object(key), ".rds")
    }
    ))

##' @importFrom R6 R6Class
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
      self$db <- remake_db$new(file.path(self$path, "db"))
      self$objects <- object_store$new(file.path(self$path, "objects"))
      self$files <- file_store$new()
      self$version <- packageVersion(.packageName)
      self$packages <- packages
      self$env <- managed_environment$new(sources)
    },

    destroy=function() {
      file_remove(self$path, recursive=TRUE)
      self$path <- NULL
      self$db <- NULL
      self$objects <- NULL
      self$files <- NULL
    },

    contains=function(name, type) {
      private$right_store(type)$contains(name)
    },

    get_hash=function(name, type, missing_ok) {
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

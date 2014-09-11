## This one has the potential issue that if the hash and the file get
## out of sync we have problems.  However, this is controlled within
## the .maker directory so we can kind of assume that is not going to
## happen.
data_store_fs <- R6::R6Class(
  "data_store_fs",
  public=list(
    path=NULL,
    initialize=function(path=".") {
      dir.create(path, FALSE)
      self$path <- file.path(normalizePath(path, mustWork=TRUE), ".maker")
      dir.create(self$path, FALSE)
    },
    destroy=function() {
      unlink(self$path, recursive=TRUE)
      self$path <- NULL
    },
    ls=function() {
      grep("__hash$", dir(self$path), invert=TRUE, value=TRUE)
    },
    contains=function(key) {
      file.exists(self$fullname(key))
    },
    store=function(key, value) {
      hash <- digest::digest(value)
      saveRDS(value, self$fullname(key))
      writeLines(hash, self$hashname(key))
    },
    get_hash=function(key) {
      h <- vapply(self$hashname(key), readLines, character(1))
      names(h) <- key
      h
    },
    get_time=function(key) {
      t <- file.info(self$fullname(key))$mtime
      names(t) <- key
      t
    },
    rm=function(key) {
      file.remove(self$fullname(key))
    },
    older_than=function(key, time) {
      self$get_time(key) < time
    },
    has_hash=function(key, hash) {
      if (length(key) != length(hash)) {
        stop("key and hash must have same length")
      }
      self$get_hash(key) == hash
    },
    ## Not all backends will support this.
    fullname=function(key) {
      file.path(self$path, key)
    },
    hashname=function(key) {
      file.path(self$path, paste0(key, "__hash"))
    }
    ))

## This one is quite different.  It also does not try to do anything
## clever with caching this information.  The files on disk are
## allowed to change independently of R.
##
## All hell is going to break loose if people use setwd() with this.
file_store <- R6::R6Class(
  "file_store",
  public=list(
    ## Very similar to the data_store_fs class:
    contains=function(filename) {
      file.exists(self$fullname(filename))
    },
    get_hash=function(filename) {
      tools::md5sum(self$fullname(filename))
    },
    get_time=function(filename) {
      ## TODO: check if exists?
      t <- file.info(self$fullname(filename))$mtime
      names(t) <- key
      t
    },
    older_than=function(key, time) {
      self$get_time(key) < time
    },
    has_hash=function(filename, hash) {
      if (length(filename) != length(hash)) {
        stop("filename and hash must have same length")
      }
      self$get_hash(filename) == hash
    },
    fullname=function(filename) {
      filename
    },
    ## Deal with directories here?  Probably not.
    rm=function(filename) {
      file.remove(self$fullname(filename))
    }
    ))

##' The data store used by maker.  This is an R6 class, and generally
##' does not need creating manually.  It's basically a pair of
##' key-value stores; one for R objects and one for files.
##' @title Data store used by maker
##' @export
##' @importFrom R6 R6Class
store <- R6::R6Class(
  "store",
  public=list(
    data=NULL,
    files=NULL,
    initialize=function(path=".") {
      self$data  <- data_store_fs$new(path)
      self$files <- file_store$new()
    }))

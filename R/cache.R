maker_cache <- R6Class(
  "maker_cache",
  public=list(
    cache=NULL,
    path=".maker/cache",

    initialize=function() {
      self$clear()
    },

    add=function(m) {
      private <- maker_private(m)
      file <- private$file
      if (!is.null(file)) {
        key <- hash_object(normalizePath(file, mustWork=TRUE))
        obj <- list(file=file, hash=private$hash, maker=m)
        self$cache[[key]] <- obj
        ## dir.create(self$path, FALSE, TRUE)
        ## saveRDS(obj, self$fullname(key))
      }
    },

    fetch=function(filename) {
      key <- hash_object(normalizePath(filename, mustWork=TRUE))
      obj <- self$cache[[key]]
      if (!is.null(obj)) {
        if (self$is_current(obj)) {
          return(obj$maker)
        }
      } else {
        ## filename_cache <- self$fullname(key)
        ## if (file.exists(filename_cache)) {
        ##   obj <- readRDS(filename_cache)
        ##   if (self$is_current(obj)) {
        ##     return(obj$maker)
        ##   }
        ## }
      }
      NULL
    },

    is_current=function(obj) {
      identical(hash_files(names(obj$hash), TRUE), obj$hash)
    },

    fullname=function(key) {
      file.path(path, paste0(key, ".rds"))
    },

    clear=function() {
      self$cache <- new.env(parent=emptyenv())
    }
    ))

cache <- maker_cache$new()

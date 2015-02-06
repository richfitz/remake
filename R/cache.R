maker_cache <- R6Class(
  "maker_cache",
  public=list(
    cache=NULL,

    initialize=function() {
      self$clear()
    },

    add=function(m) {
      private <- maker_private(m)
      file <- private$file
      if (!is.null(file)) {
        ## File here is stored just so we can identify things to
        ## delete later.
        key <- hash_object(normalizePath(file, mustWork=TRUE))
        obj <- list(file=file, maker=m)
        self$cache[[key]] <- obj
      }
    },

    fetch=function(filename, verbose, envir) {
      ## We only want to do this if the directories that we require
      ## are still there: .maker/ is sufficient as all others (db,
      ## objects) will be created as needed.
      if (file.exists(".maker") && is_directory(".maker")) {
        key <- hash_object(normalizePath(filename, mustWork=FALSE))
        obj <- self$cache[[key]]
        if (!is.null(obj) && self$is_current(obj$maker, verbose, envir)) {
          return(obj$maker)
        }
      }
      NULL
    },

    is_current=function(m, verbose, envir) {
      mp <- maker_private(m)
      identical(hash_files(names(mp$hash), TRUE), mp$hash) &&
        identical(maker_verbose(verbose), mp$verbose) &&
          identical(envir, mp$active_bindings$envir)
    },

    clear=function() {
      self$cache <- new.env(parent=emptyenv())
    }
    ))

cache <- maker_cache$new()

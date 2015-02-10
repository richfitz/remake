remake_cache <- R6Class(
  "remake_cache",
  public=list(
    cache=NULL,

    initialize=function() {
      self$clear()
    },

    add=function(m) {
      private <- remake_private(m)
      file <- private$file
      if (!is.null(file)) {
        ## File here is stored just so we can identify things to
        ## delete later.
        key <- hash_object(normalizePath(file, mustWork=TRUE))
        obj <- list(file=file, remake=m)
        self$cache[[key]] <- obj
      }
    },

    fetch=function(filename, verbose) {
      ## We only want to do this if the directories that we require
      ## are still there: .remake/ is sufficient as all others (db,
      ## objects) will be created as needed.
      if (file.exists(".remake") && is_directory(".remake")) {
        key <- hash_object(normalizePath(filename, mustWork=FALSE))
        obj <- self$cache[[key]]
        if (!is.null(obj) && self$is_current(obj$remake, verbose)) {
          return(obj$remake)
        }
      }
      NULL
    },

    is_current=function(m, verbose) {
      mp <- remake_private(m)
      identical(hash_files(names(mp$hash), TRUE), mp$hash) &&
        (is.null(verbose) || identical(remake_verbose(verbose), mp$verbose))
    },

    clear=function() {
      self$cache <- new.env(parent=emptyenv())
    }
  ))

cache <- remake_cache$new()

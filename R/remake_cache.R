remake_cache <- R6Class(
  "remake_cache",
  public=list(
    cache=NULL,

    initialize=function() {
      self$clear()
    },

    add=function(obj) {
      file <- obj$file
      if (!is.null(file)) {
        ## File here is stored just so we can identify things to
        ## delete later.
        key <- hash_object(normalizePath(file, mustWork=TRUE))
        self$cache[[key]] <- list(file=file, remake=obj)
      }
    },

    fetch=function(filename) {
      ## We only want to do this if the directories that we require
      ## are still there: .remake/ is sufficient as all others (db,
      ## objects) will be created as needed.
      if (file.exists(".remake") && is_directory(".remake")) {
        key <- hash_object(normalizePath(filename, mustWork=FALSE))
        obj <- self$cache[[key]]
        if (!is.null(obj) && self$is_current(obj$remake)) {
          return(obj$remake)
        }
      }
      NULL
    },

    is_current=function(obj) {
      identical(hash_files(names(obj$hash), TRUE), obj$hash)
    },

    clear=function() {
      self$cache <- new.env(parent=emptyenv())
    }
  ))

cache <- remake_cache$new()

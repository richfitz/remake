managed_environment <- R6Class(
  "managed_environment",
  public=list(
    packages=NULL,
    sources=NULL,
    source_files=NULL,
    source_files_hash=NULL,
    env=NULL,

    initialize=function(packages, sources) {
      self$packages <- packages
      self$sources <- sources
      self$reload()
    },

    reload=function(force=FALSE) {
      source_files <- self$find_files()
      source_files_hash <- tools::md5sum(source_files)
      reload <- (force
                 || is.null(self$env)
                 || !identical(source_files,      self$source_files)
                 || !identical(source_files_hash, self$source_files_hash))
      if (reload) {
        self$env <- new.env(parent=.GlobalEnv)
        self$source_files <- source_files
        self$source_files_hash <- source_files_hash
        self$load_packages()
        self$load_sources()
      }
      invisible(reload)
    },

    load_packages=function() {
      for (p in self$packages) {
        library(p, character.only=TRUE)
      }
    },

    load_sources=function() {
      for (f in self$source_files) {
        sys.source(f, self$env, chdir=TRUE)
      }
    },

    find_files=function() {
      if (any(!file.exists(self$sources))) {
        stop("Files not found:",
             paste(self$sources[!file.exists(self$sources)],
                   collapse=", "))
      }
      files <- as.list(self$sources)
      for (i in seq_along(files)) {
        s <- files[[i]]
        if (is_directory(s)) {
          files[[i]] <- dir(s, pattern="^.*\\.[Rr]$", full.names=TRUE)
        }
      }
      unlist(files)
    }
    ))

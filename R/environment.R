managed_environment <- R6Class(
  "managed_environment",
  public=list(
    packages=NULL,
    sources=NULL,
    source_files=NULL,
    source_files_hash=NULL,
    env=NULL,
    deps=NULL,

    initialize=function(packages, sources) {
      assert_character(packages, "packages")
      assert_character(sources,  "sources")
      if (!all(file.exists(sources))) {
        msg <- paste(sources[!file.exists(sources)], collapse=", ")
        stop("All files in 'sources' must exist.  Missing: ", msg)
      }
      self$packages <- packages
      self$sources <- sources
    },

    is_current=function(force=FALSE) {
      !(force
        || is.null(self$env)
        || !identical_map(hash_files(self$find_files()), self$source_files_hash)
        || !all(self$packages %in% .packages()))
    },

    reload=function(force=FALSE) {
      if (force || self$is_current()) {
        source_files <- self$find_files()
        source_files_hash <- hash_files(source_files)

        self$env <- new.env(parent=.GlobalEnv)
        self$source_files <- source_files
        self$load_packages()
        self$load_sources()
        self$source_files_hash <- source_files_hash
        self$deps <- code_deps$new(self$env)
      }
    },

    load_packages=function() {
      ## First, check that all packages are available:
      msg <- missing_packages(self$packages)
      if (length(msg) > 0L) {
        stop(missing_packages_condition(msg))
      }
      for (p in self$packages) {
        suppressMessages(library(p, character.only=TRUE, quietly=TRUE))
      }
    },

    load_sources=function() {
      catch_source <- function(e) {
        stop(sprintf("while sourcing '%s':\n%s", f, e$message),
             call.=FALSE)
      }
      for (f in self$source_files) {
        tryCatch(sys.source(f, self$env, chdir=TRUE, keep.source=TRUE),
                 error=catch_source)
      }
    },

    find_files=function() {
      if (any(!file.exists(self$sources))) {
        stop("Files not found:",
             paste(self$sources[!file.exists(self$sources)],
                   collapse=", "))
      }
      files <- as.list(self$sources)
      if (length(files) == 0L) {
        character(0)
      } else {
        for (i in seq_along(files)) {
          s <- files[[i]]
          if (is_directory(s)) {
            files[[i]] <- dir(s, pattern="^.*\\.[Rr]$", full.names=TRUE)
          }
        }
        unlist(files)
      }
    }
    ))

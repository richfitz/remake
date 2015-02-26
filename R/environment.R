managed_environment <- R6Class(
  "managed_environment",
  public=list(
    sources=NULL,
    source_files=NULL,
    source_files_hash=NULL,
    env=NULL,
    deps=NULL,

    initialize=function(sources) {
      assert_character(sources,  "sources")
      managed_environment_assert_sources_exist(sources)
      self$sources <- sources
    },

    is_current=function(force=FALSE) {
      !(force ||
          is.null(self$env) ||
            !identical_map(hash_files(self$find_files()),
                           self$source_files_hash))
    },

    reload=function() {
      source_files <- self$find_files()
      source_files_hash <- hash_files(source_files)
      self$env <- new.env(parent=.GlobalEnv)
      self$source_files <- source_files
      self$load_sources()
      self$source_files_hash <- source_files_hash
      self$deps <- code_deps(self$env)
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

managed_environment_assert_sources_exist <- function(files) {
  is_missing <- !file_exists(files)
  if (any(is_missing)) {
    missing_files <- files[is_missing]
    wrong_case <- file.exists(missing_files)
    if (any(wrong_case)) {
      missing_files[wrong_case] <-
        sprintf("%s (incorrect case => %s)",
                missing_files[wrong_case],
                file_real_case(missing_files[wrong_case]))
    }
    stop("Files not found:\n",
         paste(sprintf("\t- %s", missing_files), collapse="\n"))
  }
}

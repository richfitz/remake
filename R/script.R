##' @title Make a single target
##' @param target_names Vector of names of targets to build, or
##' \code{NULL} to build the default target (if specified in the
##' remakefile).
##' @param remake_file Name of the remakefile (by default
##' \code{remake.yml}).  This is passed to \code{remake()}.
##' @param filename A filename to save the resulting script into.  If
##' \code{NULL} (the default) then an a character vector is returned
##' that can be inspected.  It can also be sourced without writing to
##' file using \code{\link{source_remake_script}}.
##' @export
make_script <- function(target_names=NULL, filename=NULL,
                        remake_file="remake.yml") {
  scr <- remake_script(remake2(remake_file, load_sources=FALSE), target_names)
  if (is.null(filename)) {
    scr
  } else {
    writeLines(scr, filename)
    invisible(scr)
  }
}

remake_script <- function(m, target_name=NULL) {
  private <- remake_private(m)
  if (is.null(target_name)) {
    target_name <- private$target_default()
  }
  pkgs <- lapply(m$store$env$packages,
                 function(x) sprintf('library("%s")', x))
  srcs <- lapply(m$store$env$find_files(),
                 function(x) sprintf('source("%s")', x))
  ## Probably best to filter by "real" here?
  plan <- private$plan(target_name)
  cmds <- lapply(plan, function(i)
    target_run_fake(m$targets[[i]], for_script=TRUE))

  ## Need to make missing paths.
  files <- filter_targets_by_type(m$targets[plan], "file")
  paths <- dirname(names(files))
  ## Implicit targets exist...
  implicit <- vlapply(files, inherits, "target_file_implicit")
  ## ...so these paths must already exist...
  paths_existing <- unique(c(".", paths[implicit]))
  ## ...and these need creating:
  paths_to_create <- setdiff(unique(paths[!implicit]), paths_existing)
  cmds <- c(sprintf('dir.create("%s", FALSE, TRUE)', paths_to_create),
            cmds)
  
  src <- c(unlist(pkgs),
           unlist(srcs),
           unlist(cmds))
  class(src) <- "remake_script"
  src
}

##' @export
print.remake_script <- function(x, ...) {
  writeLines(x, ...)
}

##' Convenience function for sourcing a remake script.  This just takes
##' care of writing the character vector to a temporary file and
##' running R's \code{\link{source}} over it.  It will also source
##' other arbitrary sets of R code that are character vectors rather
##' than files.
##' @title Source a remake script
##' @param src Contents
##' @param ... Additional arguments passed to \code{\link{source}},
##' \emph{except} for the \code{file} and \code{local} arguments.
##' @param envir An environment to source into (by default the global
##' environment).
##' @return The environment into which the code is sourced,
##' invisibly.  This is primarily useful when used as
##' \code{source_remake_script(script, envir=new.env())}, as the
##' environment created in the call is returned.
##' @export
source_remake_script <- function(src, ..., envir=.GlobalEnv) {
  assert_inherits(src, c("remake_script", "character"))
  dest <- tempfile()
  writeLines(src, dest)
  on.exit(file.remove(dest))
  source(dest, envir, ...)
  invisible(envir)
}

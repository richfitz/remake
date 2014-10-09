##' @export
print.maker_script <- function(x, ...) {
  writeLines(x, ...)
}

##' Convenience function for sourcing a maker script.  This just takes
##' care of writing the character vector to a temporary file and
##' running R's \code{\link{source}} over it.  It will also source
##' other arbitrary sets of R code that are character vectors rather
##' than files.
##' @title Source a maker script
##' @param src Contents
##' @param ... Additional arguments passed to \code{\link{source}},
##' \emph{except} for the \code{file} and \code{local} arguments.
##' @param envir An environment to source into (by default the global
##' environment).
##' @return The environment into which the code is sourced,
##' invisibly.  This is primarily useful when used as
##' \code{source_maker_script(script, envir=new.env())}, as the
##' environment created in the call is returned.
##' @export
source_maker_script <- function(src, ..., envir=.GlobalEnv) {
  assert_inherits(src, c("maker_script", "character"))
  dest <- tempfile()
  writeLines(src, dest)
  on.exit(file.remove(dest))
  source(dest, envir, ...)
  invisible(envir)
}

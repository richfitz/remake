maker_environment_info <- function(e) {
  t <- attr(e, "target")
  if (is.null(t)) {
    message("Maker environment")
  } else {
    message("Maker environment for building ", t$name)
    message("    rule: ", t$rule)
    message("    command: ", target_run_fake(t))
  }
  message("    objects:")
  message(paste(strwrap(paste(ls(e), collapse=", "),
                        indent=8, exdent=8), collapse="\n"))
}

##' These functions may be useful in debugging workflows.  A
##' \code{maker_environment} object is created by running
##' \code{make_dependencies(m, target_name)}, where \code{target_name}
##' is the name of some target.  This returns an environment with all
##' the dependencies of that target so you can troubleshoot/develop
##' the rule that will process it.  The \emph{parent} environment of
##' this environment contains all the rules that maker knows about
##' (loaded from the files in the \code{sources} section of the
##' makerfile).  These functions ease interactions with these objects.
##'
##' @title Interact with "maker environment" objects
##' @param e A \code{maker_environment} object.
##' @param verbose Logical indicating if information will be printed.
##' Because these functions to odd things to your search path, this is
##' \code{TRUE} by default.
##' @export
##' @rdname maker_environment
maker_environment_browse <- function(e, verbose=TRUE) {
  assert_inherits(e, "maker_environment")
  if (verbose) {
    maker_environment_info(e)
  }
  browse_env(e)
}

##' @export
##' @rdname maker_environment
maker_attach <- function(e, verbose=TRUE) {
  assert_inherits(e, "maker_environment")
  if (verbose) {
    maker_environment_info(e)
  }
  maker_detach(warn=FALSE, verbose=verbose)
  t <- attr(e, "target")
  name <- paste0("maker:", if (is.null(t)) "<objects>" else t$name)
  attach(parent.env(e), name="maker:functions")
  attach(e, name=name)
  if (verbose) {
    message("...clean up with maker_detach()")
  }
}

##' @export
##' @rdname maker_environment
##' @param warn Logical, indicating if a warning should be given when
##' there are no maker environments to detach.
maker_detach <- function(warn=TRUE, verbose=TRUE) {
  ours <- grep("^maker:", search(), value=TRUE)
  if (length(ours) > 0L) {
    for (i in ours) {
      if (verbose) {
        message("Detaching ", i)
      }
      detach(name=i, character.only=TRUE)
    }
  } else if (warn) {
    warning("No maker environments found on search path")
  }
  invisible(length(ours) > 0L)
}

browse_env <- function(e, ...) {
  f <- function(.envir) {
    for (.obj in ls(envir=.envir, all.names=TRUE)) {
      tryCatch(assign(.obj, get(.obj, envir=e)),
               error = function(e) {})
    }
    rm(.obj, .envir)
    browser()
  }
  environment(f) <- parent.env(e)
  f(e)
}

##' @export
print.maker_environment <- function(x, ...) {
  maker_environment_info(x)
}

maker_environment <- function(m, names=character(0), target=NULL) {
  m$load_sources()
  e <- new.env(parent=m$store$env$env)
  m$store$objects$export(names, e)
  attr(e, "target") <- target
  class(e) <- "maker_environment"
  e
}

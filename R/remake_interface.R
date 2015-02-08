## Functions for interfacing with remake objects.  Not all of these
## will be supported going forward.

## TODO: I'm not sure this one is actually useful.
make_dependencies <- function(m, target_name, ...) {
  private <- remake_private(m)
  t <- private$get_target(target_name)

  private$refresh()
  private$print_message("ENV", t$name, style="angle")
  private$make1(target_name, ..., dependencies_only=TRUE)
  deps_name <- t$depends_name[t$depends_type == "object"]

  invisible(remake_environment(m, deps_name, t))
}

## This one is used for testing, but I'm not sure how useful it will
## generally be?  When working in global mode, it's possible that
## assigning NULL onto an "object" could trigger this: that'd be
## nicer perhaps?
remake_remove_target <- function(m, target_name, chain=TRUE) {
  remake_private(m)$remove_target(target_name, chain)
}

remake_target_names <- function(m, all=FALSE) {
  if (!all) {
    ok <- vlapply(m$targets, function(x) is.null(x$chain_parent))
    names(m$targets[ok])
  } else {
    names(m$targets)
  }
}

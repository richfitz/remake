## Functions for interfacing with remake objects.  Not all of these
## will be supported going forward.

## TODO: I'm not sure this one is actually useful.
make_dependencies <- function(m, target_name, ...) {
  assert_has_target(target_name, m)
  private <- remake_private(m)
  t <- m$targets[[target_name]]

  private$refresh()
  remake_print_message(m, "ENV", t$name, style="angle")
  remake_make1(m, target_name, ..., dependencies_only=TRUE)
  deps_name <- t$depends_name[t$depends_type == "object"]

  invisible(remake_environment(m, deps_name, t))
}

remake_target_names <- function(m, all=FALSE) {
  if (!all) {
    ok <- vlapply(m$targets, function(x) is.null(x$chain_parent))
    names(m$targets[ok])
  } else {
    names(m$targets)
  }
}

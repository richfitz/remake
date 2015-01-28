## This file holds code for "interactive mode".  This is going to be
## useful for building makerfiles interactively.
interactive_drop_braces <- function(expr) {
  while (length(expr) > 1L && identical(expr[[1]], quote(`{`))) {
    if (length(expr) != 2L) {
      stop("Expected non-compound expression")
    }
    expr <- expr[[2]]
  }
  expr
}

add_target <- function(m, name, expr, ...) {
  expr <- substitute(expr)
  m$interactive$targets[[name]] <-
    make_target(name, c(list(command=expr), list(...)))
  invisible(NULL)
}

add_sources <- function(m, ...) {
  sources <- c(...)
  assert_character(sources)
  m$interactive$sources <- union(m$interactive$sources, sources)
}

add_packages <- function(m, ...) {
  packages <- c(...)
  assert_character(packages)
  m$interactive$packages <- union(m$interactive$packages, packages)
}

maker_interactive <- function() {
  list(targets=empty_named_list(),
       sources=character(0),
       packages=character(0))
}

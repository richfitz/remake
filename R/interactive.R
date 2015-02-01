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
       packages=character(0),
       active=FALSE)
}

target <- function(name, expr, ...) {
  expr <- substitute(expr)
  make_target(name, c(list(command=expr), list(...)))
}

##' @export
print.target_placeholder <- function(x, ...) {
  cat(sprintf("<target %s>\n", x$name))
}

## TODO: I think the placeholder thing is overkill: what we really
## need is an active/inactive state for maker, then store everything
## in one place.  Keep the print bit, but dispatch on that based on
## the active/inactive state.
maker_add_target <- function(m, target) {
  m$interactive$targets[[target$name]] <- target
  if (!is.null(m$envir) && inherits(target, "target_object")) {
    ## TODO: Deal with situation where binding exists (will be
    ## common).  Offer a 'force' option, delete bindings, etc.
    maker_set_active_binding(m, target$name, "target")
  }
}

maker_add_sources <- function(m, value) {
  ## NOTE: The other way of doing this is by assuming that
  ## things that exist or end in .[Rrs] or a slash are sources
  ## and try to load everything else as packages?  The other
  ## option would be do allow package:testthat.
  is_source <- (grepl("\\.[rR]$", value) |
                  grepl("/", value) |
                    file.exists(value))
  m$interactive$packages <- union(m$interactive$packages,
                                  sub("^package:", "", value[!is_source]))
  m$interactive$sources <- union(m$interactive$sources,
                                 value[is_source])
  if (!is.null(m$envir)) {
    ## Here we actually want to build and reload the managed
    ## environment object.  There's some repetition here about
    ## how this should be done, but I think this will do the
    ## right thing.
    m$store$env <- managed_environment$new(m$interactive$packages,
                                           m$interactive$sources)
    ## TODO: Again, can't get at the printer without having things
    ## loaded already:
    ## m$print_message("READ", "", "# loading sources")
    m$store$env$reload(TRUE)
    maker_reload_active_bindings(m, "source")
  }
}

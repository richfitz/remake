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

maker_interactive_config <- function() {
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
  maker_interactive_list(m)$targets[[target$name]] <- target
  if (inherits(target, "target_object")) {
    obj <- maker_active_bindings(m)
    if (!is.null(obj)) {
      ## TODO: Deal with situation where binding exists (will be
      ## common).  Offer a 'force' option, delete bindings, etc.
      maker_set_active_binding(m, target$name, "target", obj)
    }
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
  dat <- maker_interactive_list(m)
  dat$packages <- union(dat$packages, sub("^package:", "", value[!is_source]))
  dat$sources <- union(dat$sources, value[is_source])
  maker_interactive_list(m) <- dat

  ## NOTE: we only rebuild the sources if running in global mode.
  obj <- maker_active_bindings(m)
  if (!is.null(obj)) {
    m$store$env$packages <- dat$packages
    m$store$env$sources  <- dat$sources
    maker_private(m)$initialize_sources()
  }
}

## Internal use only:
maker_interactive_list <- function(m) {
  maker_private(m)$interactive
}
## TODO: This is *ugly*.
`maker_interactive_list<-` <- function(m, value) {
  private <- maker_private(m)
  private$interactive <- value
  m
}

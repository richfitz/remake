.R6_remake_interactive <- R6Class(
  "remake_interactive",
  public=list(
    m=NULL,
    interactive=NULL,

    initialize=function(verbose=TRUE, envir=NULL) {
      self$interactive <- remake_interactive_config()
      self$m <- .R6_remake$new(NULL, verbose, envir)
      private$set_m_private_config(FALSE)
    },

    make=function(target_names=NULL, ...) {
      self$active <- TRUE
      self$m$make(target_names, ...)
    }
  ),

  active=list(
    add=function(value) {
      if (missing(value)) {
        message("Pass in libary/source/target calls here")
      } else  if (inherits(value, "target_base")) {
        remake_interactive_add_target(self, value)
      } else if (is.character(value)) {
        remake_interactive_add_sources(self, value)
      } else {
        stop("Can't add objects of class: ",
             paste(class(value), collapse=" / "))
      }
    },

    active=function(value) {
      if (missing(value)) {
        self$interactive$active
      } else {
        self$interactive$active <- value
        if (self$interactive$active) {
          private$set_m_private_config(TRUE)
        }
      }
    }),

  private=list(
    set_m_private_config=function(refresh=TRUE) {
      mp <- remake_private(self$m)
      mp$config <- self$interactive
      if (refresh) {
        mp$refresh()
      }
    }
  ))

## This file holds code for "interactive mode".  This is going to be
## useful for building remakefiles interactively.
interactive_drop_braces <- function(expr) {
  while (length(expr) > 1L && identical(expr[[1]], quote(`{`))) {
    if (length(expr) != 2L) {
      stop("Expected non-compound expression")
    }
    expr <- expr[[2]]
  }
  expr
}

remake_interactive_config <- function() {
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
## need is an active/inactive state for remake, then store everything
## in one place.  Keep the print bit, but dispatch on that based on
## the active/inactive state.
remake_interactive_add_target <- function(obj, target) {
  obj$interactive$targets[[target$name]] <- target
  if (inherits(target, "target_object")) {
    b <- remake_active_bindings(obj$m)
    if (!is.null(b)) {
      ## TODO: Deal with situation where binding exists (will be
      ## common).  Offer a 'force' option, delete bindings, etc.
      remake_set_active_binding(obj$m, target$name, "target", b)
    }
  }
}

remake_interactive_add_sources <- function(obj, value) {
  ## NOTE: The other way of doing this is by assuming that
  ## things that exist or end in .[Rrs] or a slash are sources
  ## and try to load everything else as packages?  The other
  ## option would be do allow package:testthat.
  is_source <- (grepl("\\.[rR]$", value) |
                  grepl("/", value) |
                    file.exists(value))
  dat <- obj$interactive
  dat$packages <- union(dat$packages, sub("^package:", "", value[!is_source]))
  dat$sources <- union(dat$sources, value[is_source])
  obj$interactive <- dat

  ## NOTE: we only rebuild the sources if running in global mode.
  if (!is.null(remake_active_bindings(obj$m))) {
    obj$m$store$env$packages <- dat$packages
    obj$m$store$env$sources  <- dat$sources
    remake_private(obj$m)$initialize_sources()
  }
}

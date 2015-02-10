.R6_remake_interactive <- R6Class(
  "remake_interactive",
  public=list(
    m=NULL,
    interactive=NULL,

    initialize=function(verbose=TRUE) {
      self$interactive <- remake_interactive_config()
      self$m <- .R6_remake$new(NULL, verbose)
      private$set_m_private_config(FALSE)
    },

    make=function(target_names=NULL, ...) {
      self$active <- TRUE
      remake_make(self$m, target_names, ...)
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

remake_interactive_add_target <- function(obj, target) {
  obj$interactive$targets[[target$name]] <- target
  if (inherits(target, "target_object")) {
    ## TODO: Set the active bindings if they are active.  Unlike
    ## sources we might need to do a bit more work because we're going
    ## to try and add just one binding (and because targets aren't as
    ## self contained as sources are).
    ##
    ## if (<active bindings are on for this object>) {
    ##   global_active_bindings$create_target_binding(target, self)
    ## }
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

  ## TODO: Set up active bindings.  In pseudo code:
  ##
  ## if (<active bindings are on for this object>) {
  ##   obj$m$store$env$packages <- dat$packages
  ##   obj$m$store$env$sources  <- dat$sources
  ##   remake_private(obj$m)$initialize_sources()
  ## }
  ##
  ## ...but this is going to require that our object is recognised in
  ## the active bindings manager, which is not yet supported.
}

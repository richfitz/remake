## This are the workhorse functions that generate the right sort of
## active binding function for binding to a target (going via
## `make1()`) or something from the managed environment (a simple
## `get()`).
make_active_binding_function <- function(m, name, type) {
  force(m)
  force(name)
  force(type)
  private <- maker_private(m)
  function(value) {
    if (missing(value)) {
      if (type == "target") {
        if (inherits(m, "maker_interactive") && !m$active) {
          ret <- maker_interactive_list(m)$targets[[name]]
          class(ret) <- c("target_placeholder", class(ret))
          ret
        } else {
          oo <- private$verbose$print_noop
          on.exit(private$verbose$print_noop <- oo)
          private$verbose$print_noop <- FALSE
          uninvisible(private$make1(name))
        }
      } else if (type == "source") {
        m$store$env$env[[name]]
      } else {
        ## TODO: This error show throw when *making* the binding, not
        ## accessing it.
        stop("Unknown type ", type)
      }
    } else {
      stop(sprintf('"%s" is managed by maker and is read-only', name),
           call.=FALSE)
    }
  }
}

maker_set_active_bindings <- function(m, type, manager, force=FALSE) {
  if (type == "target") {
    objects <- filter_targets_by_type(m$targets, "object")
    ## Exclude chain targets:
    ok <- vlapply(objects, function(x) is.null(x$chain_parent))
    names <- unname(dependency_names(objects[ok]))
  } else if (type == "source") {
    m$store$env$reload()
    names <- ls(m$store$env$env, all.names=TRUE)
  } else {
    stop("Unknown type ", type)
  }

  envir <- manager$envir
  check_active_bindings(names, envir, force)
  for (i in names) {
    makeActiveBinding(i, make_active_binding_function(m, i, type), envir)
  }

  manager$add_bindings(type, names)
  invisible(names)
}

## Similar but sets a single active binding of a single type.  The
## name is assumed valid, given its type.  This will be most useful
## for when we add targets one at a time in interactive mode and
## global mode.
maker_set_active_binding <- function(m, name, type, manager, force=FALSE) {
  envir <- manager$envir
  check_active_bindings(name, envir, force)
  makeActiveBinding(name, make_active_binding_function(m, name, type),
                    envir)
  manager$add_bindings(type, name)
}

maker_delete_active_bindings <- function(m, type, manager) {
  envir <- manager$envir
  names <- filter_active_bindings(manager$bindings[[type]], envir)
  rm(list=names, envir=envir)
  manager$bindings[[type]] <- character(0)
  invisible(names)
}

maker_purge_active_bindings <- function(manager) {
  envir <- manager$envir
  names <- filter_active_bindings(ls(envir, all.names=TRUE), envir)
  rm(list=names, envir=envir)
  for (t in names(manager$bindings)) {
    manager$clear_bindings(t)
  }
  invisible(names)
}

maker_resolve_active_bindings <- function(m, type, manager, force=FALSE) {
  envir <- manager$envir
  names <- filter_active_bindings(manager$bindings[[type]], envir)
  for (i in names) {
    if (type == "source" || force || is_current(i, m)) {
      val <- get(i, envir)
    } else {
      maker_private(m)$print_message("SKIP", i)
      val <- NULL
    }
    rm(list=i, envir=envir)
    assign(i, val, envir)
  }
  manager$clear_bindings(type)
  invisible(names)
}

filter_active_bindings <- function(names, envir, normal=FALSE) {
  names <- intersect(names, ls(envir, all.names=TRUE))
  is_binding <- vlapply(names, bindingIsActive, envir)
  names[if (normal) !is_binding else is_binding]
}

check_active_bindings <- function(names, envir, force=FALSE) {
  normal <- filter_active_bindings(names, envir, normal=TRUE)
  if (length(normal) > 0L) {
    if (force) {
      rm(list=normal, envir=envir)
    } else {
      stop("Bindngs would overwrite normal variables: ",
           paste(normal, collapse=", "))
    }
  }
}

## This helper is useful because it first deletes all the active
## bindings that it knows that it has created, and then builds a new
## set.  Of course, this is probably overkill and what we'd really
## want to do here is:
##
##   * delete all previously established bindings that would not be
##     recreated
##   * create new bindings that do not already exist
##
## But that optimisation can be made later and somewhat transparently
## to the way that it is used.
maker_reload_active_bindings <- function(m, type, manager) {
  maker_delete_active_bindings(m, type, manager)
  maker_set_active_bindings(m, type, manager)
}

## TODO: Hack for now:
maker_active_bindings <- function(m) {
  maker_private(m)$active_bindings
}

## Helper with reference semantics.  Subject to complete change.
##
## TODO: Merge the 'make_active_binding' functionality by passing in
## *functions* for each type.  These will look like this:
##   source=function(name) {m$store$env$env[[name]]}
## with m bound via local scope.
active_bindings_manager <- R6Class(
  "active_bindings_manager",
  public=list(
    envir=NULL,
    bindings=NULL,
    initialize=function(envir, types) {
      assert_environment(envir)
      self$envir=envir
      self$bindings <-
        setNames(rep(list(character(0)), length(types)), types)
    },

    add_bindings=function(type, value) {
      self$bindings[[type]] <- union(self$bindings[[type]], value)
    },

    clear_bindings=function(type) {
      self$bindings[[type]] <- character(0)
    }

  ))

maker_active_bindings_manager <- function(envir=new.env(parent=emptyenv())) {
  active_bindings_manager$new(envir, c("target", "source"))
}

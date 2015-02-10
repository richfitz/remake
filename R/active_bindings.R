##' Load bindings from \code{remake} into the global environment
##' (\code{\link{.GlobalEnv}}.  The resulting objects are "active
##' bindings" that when accessed will trigger a build of an object.
##' @title Load remake bindings into the global environment
##' @param remake_file Name of the remakefile to read.  By default
##' \code{"remake.yml"}.
##' @export
remake_create_bindings <- function(remake_file="remake.yml") {
  global_active_bindings$create_bindings(remake_file)
}
##' @export
##' @rdname remake_create_bindings
remake_delete_bindings <- function(remake_file="remake.yml") {
  global_active_bindings$delete_bindings(remake_file)
}

binding_types <- function() {
  c("source", "target")
}

## This is the workhorse function that generates the right sort of
## active binding function for binding to a target (going via
## `remake_make1()`) or something from the managed environment (a
## simple `get()`).
make_active_binding_function <- function(obj, name, type) {
  filename <- obj$file
  force(name)
  if (!(type %in% binding_types())) {
    stop("Unknown binding type ", type)
  }
  function(value) {
    if (missing(value)) {
      ## We'd fetch from cache here but that will behave badly with
      ## verbose I think.
      m <- remake(filename)
      if (type == "target") {
        if (isFALSE(obj$config$active)) {
          ret <- list(name=name)
          class(ret) <- c("target_placeholder", class(ret))
          ret
          ## TODO: We might do something different on "pause" here,
          ## such as return the last known version of the data.
        } else {
          obj$verbose$print_noop <- FALSE
          uninvisible(remake_make1(obj, name))
        }
      } else if (type == "source") {
        obj$store$env$env[[name]]
      }
    } else {
      stop(sprintf('"%s" is managed by remake and is read-only', name),
           call.=FALSE)
    }
  }
}

##' @export
print.target_placeholder <- function(x, ...) {
  cat(sprintf("<target %s>\n", x$name))
}

## This is almost certainly going to be doing too much, but I want to
## collect *all* the functionality here for now.  Some of this stuff
## will be better as free functions, really.
##
## Each manager will keep track of as single environment; this will
## get triggered by things like `remake_bindings`.  We will need a
## second manager that globally keeps track of which environment each
## remakefile is associated with, but that's a problem for later; for
## now we'll assume that the global environment is the only place
## things get put and just check there.
binding_manager <- R6Class(
  "binding_manager",
  public=list(
    envir=NULL,            # environment where bindings are stored
    files=character(0),    # files we know about
    bindings=character(0), # name of each binding
    type=character(0),     # type of each binding
    file=character(0),     # file associated with each binding

    initialize=function(envir) {
      self$envir <- envir
    },

    create_bindings=function(filename) {
      obj <- remake(filename)
      self$set_bindings("source", obj)
      self$set_bindings("target", obj)
    },

    delete_bindings=function(filename) {
      names <- self$list_bindings(filename)
      rm(list=names, envir=self$envir)
      self$drop_bindings(filename)
      invisible(names)
    },

    reload_bindings=function(type, obj) {
      file <- obj$file
      if (!is.null(file) && file %in% self$files) {
        self$set_bindings(type, obj)
      }
    },

    set_bindings=function(type, obj) {
      if (type == "target") {
        objects <- filter_targets_by_type(obj$targets, "object")
        ## Exclude chain targets:
        ok <- vlapply(objects, function(x) is.null(x$chain_parent))
        names <- unname(dependency_names(objects[ok]))
      } else if (type == "source") {
        obj$store$env$reload()
        names <- ls(obj$store$env$env, all.names=TRUE)
      } else {
        stop("Unknown type ", type)
      }

      file <- obj$file

      ## 1. Check:
      normal <- filter_active_bindings(names, self$envir, normal=TRUE)
      if (length(normal) > 0L) {
        stop("Bindngs would overwrite normal variables: ",
             paste(normal, collapse=", "))
      }

      ## 2. Check that known bindings match files
      existing <- self$bindings %in% names
      if (any(existing)) {
        if (any(self$file[existing] != file)) {
          stop("Clash of files")
        }
        if (any(self$type[existing] != type)) {
          stop("Clash of types")
        }
      }

      ## 3. Set the bindings up
      new_bindings <- setdiff(names, self$bindings)
      for (i in setdiff(names, self$bindings)) {
        makeActiveBinding(i, make_active_binding_function(obj, i, type),
                          self$envir)
      }

      ## 4. Do the book keeping
      self$files    <- union(self$files, file)
      self$bindings <- c(self$bindings, new_bindings)
      self$type     <- c(self$type, rep_len(type, length(new_bindings)))
      self$file     <- c(self$file, rep_len(file, length(new_bindings)))

      invisible(NULL)
    },

    ## Helpers:
    list_bindings=function(file) {
      filter_active_bindings(self$bindings[self$file == file], self$envir)
    },

    drop_bindings=function(file) {
      keep <- self$file != file
      self$bindings <- self$bindings[keep]
      self$file     <- self$file[keep]
      self$type     <- self$type[keep]
      self$files    <- setdiff(self$files, file)
    },

    clear=function() {
      rm(list=filter_active_bindings(self$bindings, self$envir),
         envir=self$envir)
      self$file <- character(0)
      self$type <- character(0)
      self$files <- character(0)
      self$bindings <- character(0)
    }
  ))

## One global instance at the package level:
global_active_bindings <- binding_manager$new(.GlobalEnv)

filter_active_bindings <- function(names, envir, normal=FALSE) {
  names <- intersect(names, ls(envir, all.names=TRUE))
  is_binding <- vlapply(names, bindingIsActive, envir)
  names[if (normal) !is_binding else is_binding]
}

is_active_binding <- function(sym_name, envir=.GlobalEnv) {
  exists(sym_name, envir, inherits=FALSE) && bindingIsActive(sym_name, envir)
}

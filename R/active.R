maker_set_active_bindings <- function(m, envir=.GlobalEnv) {
  make_f <- function(name) {
    force(name)
    function(value) {
      if (missing(value)) {
        ## This is not very pretty but does work: avoids printing
        ## anything if we don't actually make a target.
        oo <- m$verbose$print_noop
        on.exit(m$verbose$print_noop <- oo)
        m$verbose$print_noop <- FALSE
        m$make1(name)
      } else {
        stop(sprintf('"%s" is managed by maker and is read-only', name),
             call.=FALSE)
      }
    }
  }
  objects <- filter_targets_by_type(m$targets, "object")
  names <- unname(dependency_names(objects))
  for (i in names) {
    makeActiveBinding(i, make_f(i), envir)
  }
}

maker_delete_active_bindings <- function(m, envir=.GlobalEnv,
                                        all=FALSE) {
  objects <- maker_list_active_bindings(m, envir, all)
  rm(list=objects, envir=envir)
  invisible(objects)
}

maker_resolve_active_bindings <- function(m, envir=.GlobalEnv,
                                          all=FALSE) {
  objects <- maker_list_active_bindings(m, envir, all)
  for (i in objects) {
    val <- get(i, envir)
    rm(list=i, envir=envir)
    assign(i, val, envir)
  }
  invisible(objects)
}

maker_list_active_bindings <- function(m, envir=.GlobalEnv,
                                       all=FALSE) {
  if (all) {
    objects <- ls(envir)
  } else {
    objects <- intersect(ls(envir), m$target_names())
  }
  objects[vlapply(objects, bindingIsActive, envir)]
}

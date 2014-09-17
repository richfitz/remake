## Determine if things are up to date.  That is the case if:
##
## If the file/object does not exist it's unclean (done)
##
## If it has no dependencies it is clean (done) (no phoney targets)
##
## If the hashes of all inputs are unchanged from last time, it is clean
##
## Otherwise unclean
is_current <- function(target, store) {
  if (!store$contains(target$name, target$type)) {
    return(FALSE)
  } else if (length(target$depends) == 0L) {
    return(TRUE)
  } else if (!store$db$contains(target$name)) {
    ## This happens when a file target exists, but there is no record
    ## of it being created (such as when the .maker directory is
    ## deleted or if it comes from elsewhere).  In which case we can't
    ## tell if it's up to date and assume not.
    return(FALSE)
  } else {
    prev <- store$db$get(target$name)
    curr <- dependency_status(target, store, TRUE)
    ## In theory this is too harsh, as might also want to *remove* a
    ## dependency.  So:
    ##   i <- (sapply(prev$depends, "[[", "name") %in%
    ##         sapply(curr$depends, "[[", "name"))
    ##   prev$depends <- prev$depends[i]
    ## would filter out dependencies that have been dropped.  But that
    ## implies a change in function definition, whch should be
    ## sufficient for a rebuild.
    return(identical(prev, curr))
  }
}

## Orchestrate doing a run
do_run <- function(target, store, env) {
  get_dependency <- function(x) {
    if (x$type == "object") {
      store$objects$get(x$name)
    } else {
      x$name
    }
  }
  args <- lapply(target$depends, get_dependency)
  names(args) <- names(target$depends)
  if (!is.null(target$target_argument_name)) {
    args[[target$target_argument_name]] <- target$name
  }

  do.call(target$rule, args, envir=env)
}

dependency_status <- function(target, store, missing_ok=FALSE) {
  status1 <- function(x) {
    list(name=x$name,
         type=x$type,
         hash=unname(store$get_hash(x$name, x$type, missing_ok)))
  }
  list(name=target$name, depends=lapply(target$depends, status1))
}

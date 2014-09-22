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
  if (target$type == "cleanup" || target$type == "fake") {
    return(FALSE)
  } else if (!store$contains(target$name, target$type)) {
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
    return(compare_status(store$db$get(target$name),
                          dependency_status(target, store, missing_ok=TRUE)))
  }
}

## Orchestrate doing a run
##
## This needs some careful checking.
do_run <- function(target, store, env) {
  get_dependency <- function(x) {
    if (x$type == "object") {
      store$objects$get(x$name)
    } else {
      x$name
    }
  }
  if (target$type == "fake" || is.null(target$rule)) {
    return()
  } else if (target$type == "cleanup") {
    args <- list()
  } else {
    ## Don't depend on rules that are of special types.
    depends <- target$depends[sapply(target$depends, "[[", "type") %in%
                              c("file", "object")]
    args <- lapply(depends, get_dependency)
    names(args) <- names(depends)
    if (!is.null(target$target_argument_name)) {
      args[[target$target_argument_name]] <- target$name
    }
  }

  do.call(target$rule, args, envir=env)
}

dependency_status <- function(target, store, missing_ok=FALSE) {
  status1 <- function(x) {
    list(name=x$name,
         type=x$type,
         hash=unname(store$get_hash(x$name, x$type, missing_ok)))
  }
  list(name=target$name,
       depends=lapply(target$depends, status1),
       code=store$deps$info(target$rule))
}

## In theory this is too harsh, as might also want to *remove* a
## dependency.  So:
##   i <- (sapply(prev$depends, "[[", "name") %in%
##         sapply(curr$depends, "[[", "name"))
##   prev$depends <- prev$depends[i]
## would filter out dependencies that have been dropped.  But that
## implies a change in function definition, whch should be
## sufficient for a rebuild.
##
## TODO: An update -- because we're loading JSON we can't rely on map
## order.  Doing that with R is probably not safe anyway because these
## were sorted according to the current locale.  We'll need to
## establish a common ordering/name set for these.
compare_status <- function(prev, curr) {
  identical(prev, curr)
}

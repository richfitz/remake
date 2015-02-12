## Not implemented:
##   * check_current
##   * skip_missing
remake_environment <- function(obj, target_names, dependencies=FALSE,
                               copy_functions=FALSE,
                               delayed_assign=FALSE) {
  if (dependencies) {
    target_names <- remake_list_dependencies(obj, target_names,
                                             type="object")
  }
  e <- remake_environment_sources(obj, copy_functions)
  remake_environment_objects(obj, target_names, e, delayed_assign)
}

## This is the dumb one; it just copies things over.
remake_environment_objects <- function(obj, target_names, envir,
                                delayed_assign=FALSE) {
  object_store <- obj$store$objects
  object_store$export(target_names, envir, delayed_assign)
  invisible(envir)
}

## Then organise the sources:
remake_environment_sources <- function(obj, copy=FALSE) {
  envir_source <- obj$store$env$env
  ## TODO: Run tests with load_sources=FALSE and see what happens in
  ## this case.
  if (!is.environment(envir_source)) {
    stop("probably have to do something here?")
  }
  if (copy) {
    ## TODO: This is not actually doing the right thing: we should
    ## also set the environment of the functions to the new
    ## environment.  It might be easier to re-source into this
    ## environment?
    e <- new.env(parent=.GlobalEnv)
    copy_environment(envir_source, e)
  } else {
    e <- new.env(parent=envir_source)
  }
  e
}

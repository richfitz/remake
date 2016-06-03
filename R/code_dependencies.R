code_dependencies <- function(f, hide_error=TRUE) {
  env <- environment(f)
  ## Exclude these as they shadow variables:
  args <- names(formals(f))

  ## This is what we build up:
  ret <- list(functions=character(0),
              packages=character(0))

  walk <- function(e) {
    if (!is.recursive(e)) { # leaf
      if (!is.symbol(e)) { # A literal of some type
        return()
      }
      e_name <- deparse(e)
      ## Shadowed by argument:
      if (e_name %in% args) {
        return()
      }
      ## Can't find this function in scope; possibly global...
      if (!exists(e_name, env)) {
        return()
      }
      r <- get(e_name, env)
      if (is.function(r) && !is.primitive(r)) {
        if (identical(environment(r), env)) {
          if (!identical(r, f)) {
            ret$functions <<- c(ret$functions, e_name)
          }
        } else {
          ## TODO: for issue #85 descend into some packages here.
          r_env <- environment(r)
          if (is.environment(r_env)) {
            ret$packages <<- c(ret$packages, packageName(r_env))
          }
        }
      }
    } else { # keep going
      walk(e[[1L]]) # not sure if this is needed...
      for (a in as.list(e[-1L])) {
        if (!missing(a)) {
          walk(a)
        }
      }
    }
  }

  walk(body(f))
  lapply(ret, unique)
}

functions_in_environment <- function(env) {
  pos <- ls(env, all.names=TRUE)
  keep_if_fn <- function(x) {
    if (is.function(x)) x else NULL
  }
  obj <- lapply(pos, function(x) keep_if_fn(get(x, env)))
  names(obj) <- pos
  obj[!vlapply(obj, is.null)]
}

code_deps <- function(env) {
  fns <- functions_in_environment(env)
  deps <- lapply(fns, code_dependencies)
  functions <- lapply(deps, "[[", "functions")
  function_hashes <- lapply(fns, hash_function)

  function(x) {
    if (x %in% names(functions)) {
      fns <- sort(dependencies(x, functions))
    } else {
      fns <- character(0)
    }
    ## NOTE: This is largely future (and past) proofing in case I
    ## depend on package information or something else in the future
    ## (and to keep current caches current).  See
    ## compare_dependency_status for the only place where the list
    ## matters here.
    list(functions=function_hashes[fns])
  }
}

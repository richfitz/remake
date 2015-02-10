code_dependencies <- function(f, hide_errors=TRUE) {
  env <- environment(f)
  ## Exclude these:
  args <- names(formals(f))

  leaf <- function(e, w) {
    if (!is.symbol(e)) { # A literal of some type
      return()
    }
    e_name <- deparse(e)
    ## TODO: Can possibly exclude based on the global active binding set?
    ##
    ## TODO: I think this is excluding too much: if the variable
    ## exists in env without inheriting then we should use it, even if
    ## it would be masked at a higher level by the active binding.
    if (!exists(e_name, env)       || # local variable, probably
        e_name %in% args           || # shadowed by function argument
        is_active_binding(e_name)) {  # using remake active bindings
      return()
    }
    r <- try(eval(e, env), silent=hide_errors)
    if (!is.null(r) && is.function(r) && !is.primitive(r)) {
      if (identical(environment(r), env)) {
        if (!identical(r, f)) {
          ret$functions <<- c(ret$functions, as.character(e))
        }
      } else {
        r_env <- environment(r)
        if (is.environment(r_env)) {
          ret$packages <<- c(ret$packages, packageName(r_env))
        }
      }
    }
  }
  call <- function (e, w) {
    codetools::walkCode(e[[1]], w)
    for (a in as.list(e[-1])) {
      if (!missing(a)) {
        codetools::walkCode(a, w)
      }
    }
  }
  ret <- list(functions=character(0),
              packages=character(0))
  walker <- codetools::makeCodeWalker(call=call, leaf=leaf, write=cat)
  codetools::walkCode(body(f), walker)
  lapply(ret, unique)
}

## TODO: this looks overly complicated?
functions_in_environment <- function(env) {
  pos <- ls(env)
  keep_if_fn <- function(x) {
    if (is.function(x)) x else NULL
  }
  obj <- lapply(pos, function(x) keep_if_fn(get(x, env)))
  names(obj) <- pos
  obj[!vlapply(obj, is.null)]
}

## TODO: This can be replaced with the free function $info(rule) I
## believe.
code_deps <- R6Class(
  "code_deps",
  public=list(
    packages=NULL,
    functions=NULL,
    function_hashes=NULL,
    package_versions=NULL,

    initialize=function(env) {
      fns <- functions_in_environment(env)
      deps <- lapply(fns, code_dependencies)
      self$functions <- lapply(deps, "[[", "functions")
      self$packages  <- lapply(deps, "[[", "packages")
      self$function_hashes <- lapply(fns, hash_function)

      versions <- unname(unique(unlist(self$packages)))
      versions <- structure(lapply(versions, packageVersion), names=versions)
      self$package_versions <- lapply(versions, as.character)
    },

    depends_functions=function(x) {
      if (x %in% names(self$functions)) {
        sort(dependencies(x, self$functions))
      } else {
        character(0)
      }
    },

    depends_packages=function(x) {
      fns <- self$depends_functions(x)
      pkgs <- self$packages[fns]
      if (length(pkgs) > 0L) {
        sort(unique(unlist(pkgs)))
      } else {
        character(0)
      }
    },

    ## TODO: Really should do better with functions from another
    ## package; we *should* be able to find that on the search path
    ## starting at 'e' and then at least haul out the version
    ## information for that package.  We'll need to do that on the fly
    ## though.
    ##
    ## TODO: Plotting targets might want to depend on the remake
    ## version, or check to see if they need to be upgraded based on
    ## the remake version.
    info=function(x) {
      fns <- self$depends_functions(x)
      pkgs <- self$depends_packages(x)
      list(functions=self$function_hashes[fns],
           packages=self$package_versions[pkgs])
    }
    ))

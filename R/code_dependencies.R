code_dependencies <- function(f) {
  env <- environment(f)
  leaf <- function(e, w) {
    r <- try(eval(e, env), silent=TRUE)
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

hash_function <- function(f) {
  ## Work around a hard-to-trigger bug: this global option affects
  ## deparsing!
  oo <- options(scipen=0)
  on.exit(options(oo))
  digest::digest(deparse(f))
}

functions_in_environment <- function(env) {
  pos <- ls(env)
  keep_if_fn <- function(x) {
    if (is.function(x)) x else NULL
  }
  obj <- lapply(pos, function(x) keep_if_fn(get(x, env)))
  names(obj) <- pos
  obj[!vlapply(obj, is.null)]
}

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

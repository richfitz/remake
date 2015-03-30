## TODO: According to JJ, there is support for doing this sort of
## thing in a DTL package - possibly called 'codedeps'.  Probably
## worth finding out how that works and seeing if I can make this
## faster and more reliable.
code_dependencies <- function(f, hide_errors=TRUE) {
  env <- environment(f)
  ## Exclude these:
  args <- names(formals(f))

  leaf <- function(e, w) {
    if (!is.symbol(e)) { # A literal of some type
      return()
    }
    e_name <- deparse(e)

    ## Shadowed by argument:
    ## TODO: Nested function definitions will not work correctly
    ## here; we'd need to switch the "active" function.  Getting this
    ## wrong is most likely to cause false positives.
    if (e_name %in% args) {
      return()
    }

    ## TODO: Things like get() will totally confuse this.
    if (!exists(e_name, env, inherits=FALSE) &&
        ((!exists(e_name, env)       || # local variable, probably
          is_active_binding(e_name)))) {  # using remake active bindings
      return()
    }
    r <- try(eval(e, env), silent=hide_errors)
    if (!is.null(r) && is.function(r) && !is.primitive(r)) {
      if (identical(environment(r), env)) {
        if (!identical(r, f)) {
          ## TODO: Why not e_name here?
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
    list(functions=function_hashes[fns])
  }
}

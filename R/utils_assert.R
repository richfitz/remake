## Why not use assert_that() here?  It's possibly a bit slow:
##   microbenchmark(assert_that(is.numeric(1)), assert_numeric(1))
## Lazy evaluation saves us most of the time, but most of the time in
## assert_that is spent on carefully evaluating things.  I'm open to
## moving to it.
assert_inherits <- function(x, what, name=deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("%s must be a %s", name,
                 paste(what, collapse=" / ")), call.=FALSE)
  }
}

assert_function <- function(x, name=deparse(substitute(x))) {
  if (!is.function(x)) {
    stop(sprintf("%s must be a function", name), call.=FALSE)
  }
}

assert_null <- function(x, name=deparse(substitute(x))) {
  if (!is.null(x)) {
    stop(sprintf("%s must be NULL", name), call.=FALSE)
  }
}

assert_environment <- function(x, name=deparse(substitute(x))) {
  if (!is.environment(x)) {
    stop(sprintf("%s must be an environment", name), call.=FALSE)
  }
}

assert_list <- function(x, name=deparse(substitute(x))) {
  if (!is.list(x)) {
    stop(sprintf("%s must be a list", name), call.=FALSE)
  }
}

assert_nonnegative <- function(x, name=deparse(substitute(x))) {
  if (x < 0) {
    stop(sprintf("%s must be nonnegative", name), call.=FALSE)
  }
}

assert_numeric <- function(x, name=deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(sprintf("%s must be numeric", name), call.=FALSE)
  }
}

assert_character <- function(x, name=deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("%s must be character", name), call.=FALSE)
  }
}

assert_length <- function(x, n, name=deparse(substitute(x))) {
  if (length(x) != n) {
    stop(sprintf("%s must have %d elements", name, n), call.=FALSE)
  }
}

assert_integer <- function(x, strict=FALSE, name=deparse(substitute(x))) {
  if (!(is.integer(x))) {
    usable_as_integer <-
      !strict && is.numeric(x) && (max(abs(as.integer(x) - x)) < 1e-8)
    if (!usable_as_integer) {
      stop(sprintf("%s must be integer", name), call.=FALSE)
    }
  }
}

## Useful for things handled with size_t, though these are passed
## through a function that will also warn.  This function is preferred
## though as it generates more useful error messages -- the compiled
## one prevents crashes!
assert_size <- function(x, strict=FALSE, name=deparse(substitute(x))) {
  assert_integer(x, strict, name)
  assert_nonnegative(x, name)
}

assert_logical <- function(x, name=deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("%s must be logical", name), call.=FALSE)
  }
}

assert_scalar <- function(x, name=deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("%s must be a scalar", name), call.=FALSE)
  }
}

assert_nonempty <- function(x, name=deparse(substitute(x))) {
  if (length(x) == 0) {
    stop(sprintf("%s must not be empty", name), call.=FALSE)
  }
}

assert_scalar_list <- function(x, name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_list(x, name)
}

assert_scalar_numeric <- function(x, name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_numeric(x, name)
}

assert_scalar_integer <- function(x, strict=FALSE,
                                  name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_integer(x, strict, name)
}

assert_scalar_logical <- function(x, name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
}

assert_scalar_character <- function(x, name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}

assert_scalar_size <- function(x, strict=FALSE,
                               name=deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_size(x, strict, name)
}

assert_named <- function(x,
                         empty_can_be_unnamed=TRUE,
                         unique_names=TRUE,
                         name=deparse(substitute(x))) {
  nx <- names(x)
  if (is.null(nx) || any(nx == "")) {
    if (length(x) > 0 || !empty_can_be_unnamed) {
      stop(sprintf("%s must be named", name), call.=FALSE)
    }
  } else {
    stop_if_duplicated(nx, sprintf("%s must have unique names", name))
  }
}

assert_named_list <- function(x, empty_can_be_unnamed=TRUE,
                              unique_names=TRUE,
                              name=deparse(substitute(x))) {
  assert_named(x, empty_can_be_unnamed, unique_names, name)
  assert_list(x, name)
}

assert_named_character <- function(x, empty_can_be_unnamed=TRUE,
                                   unique_names=TRUE,
                                   name=deparse(substitute(x))) {
  assert_named(x, empty_can_be_unnamed, unique_names, name)
  assert_character(x, name)
}

assert_file_exists <- function(x, name=deparse(substitute(x))) {
  if (!file.exists(x)) {
    stop(sprintf("The file '%s' does not exist", x), call.=FALSE)
  }
}

assert_directory <- function(x, name=deparse(substitute(x))) {
  if (!file.exists(x) || !is_directory(x)) {
    stop(sprintf("'%s' is not a directory", x), call.=FALSE)
  }
}

## remake-specific assertions.  All non-api.
assert_remake_archive <- function(filename) {
  if (!is_archive(filename)) {
    stop(sprintf("'%s' is not a remake archive", filename))
  }
}

assert_has_targets <- function(target_names, obj) {
  if (!all(target_names %in% names(obj$targets))) {
    stop("No such target ",
         paste(setdiff(target_names, names(obj$targets)),
               collapse=", "))
  }
}

assert_is_current <- function(obj, target_names, check=NULL) {
  ok <- remake_is_current(obj, target_names)
  if (!all(ok)) {
    stop("Target not current: ", paste(target_names[!ok], collapse=", "))
  }
}

assert_is_url <- function(x, name=deparse(substitute(x))) {
  assert_scalar_character(x, name)
  if (!grepl("^https?://", x)) {
    stop(sprintf("%s does not look like a http/https URL", name))
  }
}

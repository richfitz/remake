## Copied from RcppR6
read_file <- function(...) {
  paste(readLines(...), collapse="\n")
}

## https://github.com/viking/r-yaml/issues/5#issuecomment-16464325
yaml_load <- function(string) {
  ## More restrictive true/false handling.  Only accept if it maps to
  ## full true/false:
  handlers <- list('bool#yes' = function(x) {
    if (identical(toupper(x), "TRUE")) TRUE else x},
                   'bool#no' = function(x) {
    if (identical(toupper(x), "FALSE")) FALSE else x})
  yaml::yaml.load(string, handlers=handlers)
}

yaml_read <- function(filename) {
  yaml_load(read_file(filename))
}

with_default <- function(x, default=NULL) {
  if (is.null(x)) default else x
}

## Warn if keys are found in an object that are not in a known set.
warn_unknown <- function(name, defn, known) {
  unknown <- setdiff(names(defn), known)
  if (length(unknown) > 0) {
    warning(sprintf("Unknown fields in %s: %s",
                    name, paste(unknown, collapse=", ")),
            immediate.=TRUE)
  }
}

## Pattern where we have a named list and we want to call function
## 'FUN' with rather than just
##    {FUN(X[[1]], ...), ..., FUN(X[[n]], ...)}
## instead as
##    {FUN{names(X)[1], X[[1]], ...}, ..., names(X)[1], X[[1]], ...}
## this can be achived via mapply, but it's not pleasant.
lnapply <- function(X, FUN, ...) {
  nX <- names(X)
  res <- lapply(seq_along(X), function(i) FUN(nX[[i]], X[[i]], ...))
  names(res) <- nX
  res
}

is_directory <- function(path) {
  file.info(path)$isdir
}

## Like match.arg(), but does not allow for abbreviation.
match_value <- function(arg, choices, msg=NULL) {
  assert_scalar_character(arg)
  if (!(arg %in% choices)) {
    stop("'arg' must be one of ", paste(dQuote(choices), collapse=", "))
  }
  arg
}

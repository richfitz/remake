## Copied from RcppR6
read_file <- function(filename, ...) {
  assert_file_exists(filename)
  paste(readLines(filename), collapse="\n")
}

## https://github.com/viking/r-yaml/issues/5#issuecomment-16464325
yaml_load <- function(string) {
  ## More restrictive true/false handling.  Only accept if it maps to
  ## full true/false:
  handlers <- list("bool#yes" = function(x) {
    if (identical(toupper(x), "TRUE")) TRUE else x},
                   "bool#no" = function(x) {
    if (identical(toupper(x), "FALSE")) FALSE else x})
  yaml::yaml.load(string, handlers=handlers)
}

yaml_read <- function(filename) {
  catch_yaml <- function(e) {
    stop(sprintf("while reading '%s'\n%s", filename, e$message),
         call.=FALSE)
  }
  tryCatch(yaml_load(read_file(filename)),
           error=catch_yaml)
}

with_default <- function(x, default=NULL) {
  if (is.null(x)) default else x
}

## Warn if keys are found in an object that are not in a known set.
stop_unknown <- function(name, defn, known, error=TRUE) {
  unknown <- setdiff(names(defn), known)
  if (length(unknown) > 0) {
    msg <- sprintf("Unknown fields in %s: %s",
                   name, paste(unknown, collapse=", "))
    if (error) {
      stop(msg, call.=FALSE)
    } else {
      warning(msg, immediate.=TRUE, call.=FALSE)
    }
  }
}

warn_unknown <- function(name, defn, known) {
  stop_unknown(name, defn, known, FALSE)
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
match_value <- function(arg, choices, name=deparse(substitute(arg))) {
  assert_scalar_character(arg)
  if (!(arg %in% choices)) {
    stop(sprintf("%s must be one of %s",
                 name, paste(dQuote(choices), collapse=", ")))
  }
  arg
}

from_yaml_map_list <- function(x) {
  if (length(x) == 0L || is.character(x)) {
    x <- as.list(x)
  } else if (is.list(x)) {
    if (!all(viapply(x, length) == 1L)) {
      stop("Expected all elements to be scalar")
    }
    x <- unlist(x, FALSE)
  } else {
    stop("Unexpected input")
  }
  x
}

abbreviate <- function(str, width, cutoff="...") {
  assert_scalar_character(str)
  nc <- nchar(str)
  if (nc <= width) {
    str
  } else if (width < nchar(cutoff)) {
    character(0)
  } else {
    w <- nchar(cutoff)
    paste0(substr(str, 1, width - w), cutoff)
  }
}

empty_named_list <- function() {
  structure(list(), names=character(0))
}
empty_named_character <- function() {
  structure(character(0), names=character(0))
}
empty_named_integer <- function() {
  structure(integer(), names=character(0))
}

strip_whitespace <- function(str) {
  gsub("(^\\s+|\\s+$)", "", str)
}

strrep <- function (str, n) {
  paste(rep_len(str, n), collapse = "")
}

last <- function(x) {
  x[[length(x)]]
}
`last<-` <- function(x, value) {
  x[[length(x)]] <- value
  x
}

insert_at <- function(x, value, pos) {
  assert_scalar_integer(pos)
  len <- length(x)
  if (pos > 0 && pos <= len) {
    i <- seq_along(x)
    x <- c(x[i < pos], value, x[i >= pos])
  } else if (pos == len + 1L) {
    x[pos] <- value
  } else {
    stop("Invalid position to insert")
  }
  x
}

isFALSE <- function(x) {
  identical(x, FALSE)
}

## NOTE: Does not handle vectors & throw warning at `if (exists)`
file_remove <- function(path, recursive=FALSE) {
  exists <- file.exists(path)
  if (exists) {
    if (is_directory(path)) {
      if (recursive) {
        unlink(path, recursive)
      } else {
        stop("Use 'recursive=TRUE' to delete directories")
      }
    } else {
      file.remove(path)
    }
  }
  invisible(exists)
}

brackets <- function(text, style="square", pad=1) {
  styles <- list(square = c("[", "]"),
                 round  = c("(", ")"),
                 curly  = c("{", "}"),
                 angle  = c("<", ">"),
                 pipe   = c("|", "|"),
                 star   = c("*", "*"),
                 none   = c(" ", " "))
  style <- styles[[match_value(style, names(styles))]]
  pad <- strrep(" ", pad)
  paste0(style[[1]], pad, text, pad, style[[2]])
}

path_copy <- function(from, to, ...) {
  dest <- file.path(to, dirname(from))
  dir.create(dest, FALSE, TRUE)
  file_copy(from, dest, ...)
}

## Needs making more robust.  Something along the lines of pythons
## os.path would be ideal I think.
path_split <- function(x) {
  strsplit(x, "/", fixed=TRUE)
}

file_copy <- function(from, to, ..., warn=TRUE) {
  assert_scalar_character(from)
  ok <- file.exists(from) && file.copy(from, to)
  if (warn && any(!ok)) {
    warning("Failed to copy file: ", paste(from[!ok], collapse=", "))
  }
  invisible(ok)
}

## This zips up the directory at `path` into basename(path).zip.
## Because of the limitations of `zip()`, we do need to change working
## directories temporarily.
## TODO: Is this generally useful?
zip_dir <- function(path, zipfile=NULL, ..., flags="-r9X", quiet=TRUE,
                    overwrite=TRUE) {
  assert_directory(path)
  at <- dirname(path)
  base <- basename(path)
  if (is.null(zipfile)) {
    zipfile <- paste0(base, ".zip")
  }
  if (quiet && !grepl("q", flags)) {
    flags <- paste0(flags, "q")
  }
  cwd <- getwd()
  zipfile_full <- file.path(cwd, zipfile)
  ## Should backup?
  if (overwrite && file.exists(zipfile)) {
    file_remove(zipfile)
  }
  if (at != ".") {
    owd <- setwd(at)
    on.exit(setwd(owd))
  }
  zip(zipfile_full, base, flags, ...)
  invisible(zipfile)
}

## For use with tryCatch and withCallingHandlers
catch_error_prefix <- function(prefix) {
  force(prefix)
  function(e) {
    e$message <- paste0(prefix, e$message)
    stop(e)
  }
}
catch_warning_prefix <- function(prefix) {
  force(prefix)
  function(e) {
    e$message <- paste0(prefix, e$message)
    warning(e)
    invokeRestart("muffleWarning")
  }
}

rep_along <- function(x, along.with) {
  rep_len(x, length(along.with))
}

backup <- function(file) {
  if (file.exists(file)) {
    path <- file.path(tempfile(), file)
    dir.create(dirname(path), showWarnings=FALSE, recursive=TRUE)
    file.copy(file, path)
    path
  } else {
    NULL
  }
}

restore <- function(file, path) {
  if (!is.null(path)) {
    message("Restoring previous version of ", file)
    file.copy(path, file, overwrite=TRUE)
  }
}

file_extension <- function(x) {
  pos <- regexpr("\\.([^.]+)$", x, perl=TRUE)
  ret <- rep_along("", length(x))
  i <- pos > -1L
  ret[i] <- substring(x[i], pos[i] + 1L)
  ret
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

uninvisible <- function(x) {
  force(x)
  x
}

## Like dQuote but not "smart"
dquote <- function(x) {
  sprintf('"%s"', x)
}
squote <- function(x) {
  sprintf("'%s'", x)
}

append_lines <- function(text, file) {
  assert_character(text)
  assert_scalar_character(file)
  if (file.exists(file)) {
    existing <- readLines(file)
  } else {
    existing <- character(0)
  }
  writeLines(c(existing, text), file)
}

## Attempt to work out if git ignores a set of files.  Returns a
## logical vector along the set.  If git is not installed, if we're
## not in a git repo, or if there is an error running `git
## check-ignore`, then all files are assumed not to be ignored.
git_ignores <- function(files) {
  if (length(files) == 0L || !git_exists()) {
    rep_along(FALSE, files)
  } else {
    tmp <- tempfile()
    on.exit(file_remove(tmp))
    writeLines(files, tmp)

    recover_git <- function(e) {
      warning(e)
      character(0)
    }
    ignored <- tryCatch(system2("git", c("check-ignore", "--stdin"),
                                stdin=tmp, stdout=TRUE, stderr=FALSE),
                        condition=recover_git)
    files %in% ignored
  }
}

## Checks that git exists *and* that we're running in a git repo.
git_exists <- function() {
  res <- tryCatch(git_sha(), condition=function(e) e)
  !inherits(res, "condition")
}

git_sha <- function() {
  system2("git", c("rev-parse", "HEAD"), stdout=TRUE, stderr=FALSE)
}

copy_environment <- function(from, to) {
  for (i in ls(from, all.names=TRUE)) {
    assign(i, get(i, from), to)
  }
}

## Not sure about this one...
browse_environment <- function(e, ...) {
  f <- function(.envir) {
    for (.obj in ls(envir=.envir, all.names=TRUE)) {
      tryCatch(assign(.obj, get(.obj, envir=e)),
               error = function(e) {})
    }
    rm(.obj, .envir)
    browser()
  }
  environment(f) <- parent.env(e)
  f(e)
}

##' @importFrom crayon make_style
paint <- function(str, col) {
  if (is.null(col)) {
    str
  } else {
    make_style(col)(str)
  }
}

did_you_mean <- function(name, pos, prefix="did you mean: ") {
  close <- vcapply(name, function(x)
    paste(agrep(name, pos, ignore.case=TRUE, value=TRUE), collapse=", "))
  i <- nchar(close) > 0
  if (!is.null(prefix)) {
    close[i] <- paste0(prefix, close[i])
  }
  unname(close)
}

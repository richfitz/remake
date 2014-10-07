## Copied from RcppR6
read_file <- function(filename, ...) {
  assert_file_exists(filename)
  paste(readLines(filename), collapse="\n")
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
    if (!all(sapply(x, length) == 1L)) {
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

file_copy <- function(from, to, ..., warn=TRUE) {
  ok <- file.copy(from, to)
  if (warn && any(!ok)) {
    warning("Failed to copy file: ", paste(from[!ok], collapse=", "))
  }
  invisible(ok)
}

## This zips up the directory at `path` into basename(path).zip.
## Because of the limitations of `zip()`, we do need to change working
## directories temporarily.
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
    file.remove(zipfile)
  }
  if (at != ".") {
    owd <- setwd(at)
    on.exit(setwd(owd))
  }
  zip(zipfile_full, base, flags, ...)
  invisible(zipfile)
}

## This is just to avoid dealing with .onLoad
painter <- R6Class(
  public=list(
    do_paint=NULL,
    normal_is_bright=NULL,
    initialize=function(normal_is_bright=FALSE) {
      has_rainbowrite <- requireNamespace("rainbowrite", quietly=TRUE)
      self$normal_is_bright <- normal_is_bright
      if (has_rainbowrite) {
        self$do_paint <- rainbowrite::paint
      } else {
        self$do_paint <- function(x, ...) x
      }
    },
    paint=function(..., normal_is_bright=self$normal_is_bright) {
      self$do_paint(..., normal_is_bright=normal_is_bright)
    }
    ))

## * `packages`: vector of packages to read
## * `sources`: vector of source files to read
## * `default_target`: name of the default target (within `targets`)
## * `targets`: an unordered dictionary of targets to build.  Each
##   target has form:
##
## name:
##   rule: generating_function
##   depends: <see below>
##
## The name is a character string and must be unique within this file.
## If it contains a slash (/) or ends in one of the filename
## extensions it will be assumed to be a file.  Otherwise it is
## assumed to be the identifier for an R object.

read_maker <- function(filename) {
  validate_maker(yaml_read(filename), filename)
}

validate_maker <- function(dat, filename) {
  warn_unknown(filename, dat, c("packages", "sources", "targets"))

  dat$packages <- with_default(dat$packages, character(0))
  dat$sources  <- with_default(dat$sources,  character(0))

  if (!is.character(dat$packages)) {
    stop("'packages' must be a character vector")
  }
  if (!is.character(dat$sources)) {
    stop("'sources' must be a character vector")
  }
  if (!all(file.exists(dat$sources))) {
    stop("All files in 'sources' must exist")
  }
  if (any(duplicated(names(dat$targets)))) {
    stop("All target names must be unique")
  }
  dat$targets <- lnapply(dat$targets, validate_target)

  for (t in dat$targets) {
    t$initialise_depends(dat)
   }
  dat
}

validate_target <- function(target_name, obj) {
  warn_unknown(target_name, target,
               c("rule", "depends", "target_argument_name"))
  target$new(target_name, obj$rule, obj$depends, obj$target_argument_name)
}

target <- R6::R6Class(
  "target",
  public=list(
    name=NULL,
    depends=NULL,
    rule=NULL,
    type=NULL,
    target_argument_name=NULL,
    initialize=function(name, rule, depends=NULL, target_argument_name=NULL) {
      self$name <- name
      self$type <- if (target_is_file(name)) "file" else "object"

      if (self$type == "object" && !is.null(target_argument_name)) {
        stop("'target_argument_name' is only allowed for file targets")
      }
      ## TODO: Should check that this name is not used as a name of
      ## self$depends.
      self$target_argument_name <- target_argument_name

      assert_scalar_character(rule)
      self$rule <- rule

      ## These get wired up as actual maker::target objects on a
      ## second pass, but we need a full database to do that.
      self$depends <- as.list(depends)
    },

    initialise_depends=function(obj) {
      sapply(self$depends, assert_scalar_character)
      depends_name <- sapply(self$depends, "[[", 1)
      if (any(duplicated(names(depends_name)))) {
        stop("Dependency listed more than once")
      }
      if (any(duplicated(setdiff(names(self$depends), "")))) {
        stop("All named depends targets must be unique")
      }
      i <- match(depends_name, names(obj$targets))
      if (any(is.na(i))) {
        stop(sprintf("Unknown dependencies of %s: %s",
                     self$name,
                     paste(obj$depends[is.na(i)], collapse=", ")))
      }
      ## This preserves the original names:
      self$depends <- as.list(self$depends)
      self$depends[] <- unname(obj$targets[i])
    }
    ))


##' Returns the vector of known file extensions.  If a target ends in
##' one of these, then it will be considered a file, rather than an
##' object.
##' @title Vector of file extensions
##' @export
extensions <- function() {
  c(# Data
    "csv", "tsv", "xls", "xlsx", "rds", "rda", "rdata",
    # Free form
    "txt", "log", "yml", "yaml", "xml",
    # Text
    "md", "tex",
    # Graphics
    "jpg", "jpeg", "png", "pdf", "eps", "ps", "bmp", "tiff", "svg",
    # Archives
    "zip", "gz", "tar", "bz2")
}

##' Determine if a target is treated as a file or not.
##'
##' A target is a file if it contains a slash anywhere in the name, or
##' if it ends in one of the known extensions.  The current set of
##' known file extensions is available as \code{\link{extensions}()},
##' but soon will become configurable.
##' @title Determine if target is a file
##' @param x Vector of target names
##' @return A logical vector, the same length as \code{x}
##' @export
target_is_file <- function(x) {
  ext_pattern <- sprintf("\\.(%s)$", paste(extensions(), collapse="|"))
  grepl("/", x) | grepl(ext_pattern, x, ignore.case=TRUE)
}

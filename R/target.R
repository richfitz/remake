target <- R6Class(
  "target",
  public=list(
    name=NULL,
    depends=NULL,
    rule=NULL,
    type=NULL,
    cleanup=NULL,
    target_argument_name=NULL,
    implicit=NULL,
    initialize=function(name, type, rule, depends=NULL, cleanup="tidy",
      target_argument_name=NULL, implicit=FALSE) {
      #
      self$name <- name
      self$type <- type
      self$implicit <- implicit
      if (is.null(rule) || type == "cleanup") {
        cleanup <- "never"
      }
      self$cleanup <- match_value(cleanup, cleanup_levels())

      if (self$type == "object" && !is.null(target_argument_name)) {
        stop("'target_argument_name' is only allowed for file targets")
      }
      ## TODO: Should check that this name is not used as a name of
      ## self$depends.
      self$target_argument_name <- target_argument_name

      if (is.null(rule)) {
        if (self$type == "file" && !file.exists(name)) {
          warning("Creating NULL target for nonexistant file ", name)
        }
      } else {
        assert_scalar_character(rule)
      }
      self$rule <- rule

      ## These get wired up as actual maker::target objects on a
      ## second pass, but we need a full database to do that.
      self$depends <- as.list(depends)
    },

    initialise_depends=function(obj) {
      if (length(self$depends) == 0L) {
        return()
      }

      sapply(self$depends, assert_scalar_character)
      depends_name <- sapply(self$depends, "[[", 1)
      if (any(duplicated(names(depends_name)))) {
        stop("Dependency listed more than once")
      }
      if (any(duplicated(setdiff(names(self$depends), "")))) {
        stop("All named depends targets must be unique")
      }

      ## This section matches the dependencies with their location in
      ## the database's set of targets. Missing file targets will be
      ## created and added to the database (which being passed by
      ## reference will propagate backwards).
      msg <- setdiff(depends_name, obj$target_names())
      if (length(msg) > 0L) {
        obj$add_targets(lapply(msg, target$new, type=target_type(msg),
                               rule=NULL, implicit=TRUE))
      }

      ## This preserves the original names:
      self$depends[] <- obj$get_targets(depends_name)
    },

    dependencies=function() {
      sapply(self$depends, function(x) x$name)
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

target_type <- function(x) {
  ifelse(target_is_file(x), "file", "object")
}

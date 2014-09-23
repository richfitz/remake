target <- R6Class(
  "target",
  public=list(
    name=NULL,
    depends=NULL,
    rule=NULL,
    type=NULL,
    cleanup_level=NULL,
    target_argument_name=NULL,
    implicit=NULL,
    ## TODO: the proliferation of similar cases here is not great.
    ## Think of a way of splitting the class into smaller pieces.
    plot=NULL,
    maker=NULL,

    initialize=function(name, type, rule, depends=NULL, cleanup_level="tidy",
      target_argument_name=NULL, implicit=FALSE) {
      #
      self$name <- name
      self$type <- type
      self$implicit <- implicit
      if (is.null(rule) || type %in% c("cleanup", "fake")) {
        cleanup_level <- "never"
      }
      self$cleanup_level <- match_value(cleanup_level, cleanup_levels())

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
      self$depends <- from_yaml_map_list(depends)
    },

    activate=function(maker) {
      self$maker <- maker
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
      msg <- setdiff(depends_name, self$maker$target_names())
      if (length(msg) > 0L) {
        target_implicit <- function(name) {
          target$new(name, target_type(name), NULL, implicit=TRUE)
        }
        self$maker$add_targets(lapply(msg, target_implicit))
      }

      ## This preserves the original names:
      self$depends[] <- self$maker$get_targets(depends_name)
    },

    is_active=function() {
      !is.null(self$maker)
    },

    get=function() {
      if (self$type == "object") {
        self$maker$store$objects$get(self$name)
      } else if (self$type == "file") {
        self$name
      } else {
        stop("Not a target that can be got")
      }
    },

    get_fake=function() {
      if (self$type == "object") {
        self$name
      } else if (self$type == "file") {
        sprintf('"%s"', self$name)
      } else {
        stop("Not a target that can be got")
      }
    },

    set=function(value) {
      if (self$type %in% c("cleanup", "fake")) {
        return()
      }
      if (self$type == "object") {
        self$maker$store$objects$set(self$name, value)
      }
      self$maker$store$db$set(self$name, self$dependency_status())
    },

    del=function(missing_ok=FALSE) {
      ## This does also take care of cleaning out the daabase.
      self$maker$store$del(self$name, self$type, missing_ok)
    },

    is_current=function() {
      is_current(self, self$maker$store)
    },

    status_string=function(current=NULL) {
      if (is.null(current)) {
        current <- self$is_current()
      }
      if (self$type == "cleanup") {
        "CLEAN"
      } else if (is.null(self$rule)) {
        ""
      } else if (current) {
        "OK"
      } else {
        "BUILD"
      }
    },

    dependencies=function() {
      sapply(self$depends, function(x) x$name)
    },

    dependency_status=function(missing_ok=FALSE) {
      dependency_status(self, self$maker$store, missing_ok=missing_ok)
    },

    dependencies_as_args=function(fake=FALSE) {
      if (self$type == "fake" || is.null(self$rule)) {
        NULL
      } else if (self$type == "cleanup") {
        list()
      } else {
        ## Don't depend on rules that are of special types.
        dep_type <- sapply(self$depends, "[[", "type")
        depends <- self$depends[dep_type %in% c("file", "object")]
        if (fake) {
          args <- lapply(depends, function(x) x$get_fake())
        } else {
          args <- lapply(depends, function(x) x$get())
        }
        names(args) <- names(depends)
        if (!is.null(self$target_argument_name)) {
          args[[self$target_argument_name]] <-
            if (fake) self$get_fake() else self$get()
        }
        args
      }
    },

    run=function() {
      if (self$dont_run()) {
        return()
      }
      args <- self$dependencies_as_args()
      if (!is.null(self$plot)) {
        open_device(self$name, self$plot$device, self$plot$args,
                    self$maker$env)
        on.exit(dev.off())
      }
      res <- do.call(self$rule, args, envir=self$maker$env)
      self$set(res)
      invisible(res)
    },

    run_fake=function() {
      if (self$dont_run()) {
        return(character(0))
      } else if (self$type == "cleanup") {
        ## TODO: Will need to fill this in at some point
        return(character(0))
      } else {
        args <- unlist(self$dependencies_as_args(fake=TRUE))
        if (is.null(names(args))) {
          args <- args
        } else {
          args <- ifelse(names(args) == "",
                         args, paste(names(args), args, sep="="))
        }
        args <- paste(args, collapse=", ")
        str <- sprintf("%s(%s)", self$rule, args)
        if (self$type == "object") {
          str <- paste(self$name, "<-", str)
        } else if (!is.null(self$plot)) {
          str <- paste(str, "==>", self$name)
        }
        str
      }
    },

    build=function() {
      ## TODO: This is an awkward callback:
      if (self$type == "cleanup") {
        self$maker$cleanup(self$name)
      }
      if (self$implicit) {
        stop("Can't build implicit targets")
      }
      ## This avoids either manually creating directories, or obscure
      ## errors when R can't save a file to a place.  Possibly this
      ## should be a configurable behaviour, but we're guaranteed to
      ## be working with genuine files so this should be harmless.
      if (self$type == "file") {
        dir.create(dirname(self$name), showWarnings=FALSE, recursive=TRUE)
      }
      self$run()
    },

    dont_run=function() {
      self$type == "fake" || is.null(self$rule)
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

## Determine if things are up to date.  That is the case if:
##
## If the file/object does not exist it's unclean (done)
##
## If it has no dependencies it is clean (done) (no phoney targets)
##
## If the hashes of all inputs are unchanged from last time, it is clean
##
## Otherwise unclean
is_current <- function(target, store) {
  if (target$type == "cleanup" || target$type == "fake") {
    return(FALSE)
  } else if (!store$contains(target$name, target$type)) {
    return(FALSE)
  } else if (length(target$depends) == 0L) {
    return(TRUE)
  } else if (!store$db$contains(target$name)) {
    ## This happens when a file target exists, but there is no record
    ## of it being created (such as when the .maker directory is
    ## deleted or if it comes from elsewhere).  In which case we can't
    ## tell if it's up to date and assume not.
    return(FALSE)
  } else {
    return(compare_status(store$db$get(target$name),
                          dependency_status(target, store, missing_ok=TRUE)))
  }
}

dependency_status <- function(target, store, missing_ok=FALSE) {
  status1 <- function(x) {
    list(name=x$name,
         type=x$type,
         hash=unname(store$get_hash(x$name, x$type, missing_ok)))
  }
  list(name=target$name,
       depends=lapply(target$depends, status1),
       code=store$deps$info(target$rule))
}

## In theory this is too harsh, as might also want to *remove* a
## dependency.  So:
##   i <- (sapply(prev$depends, "[[", "name") %in%
##         sapply(curr$depends, "[[", "name"))
##   prev$depends <- prev$depends[i]
## would filter out dependencies that have been dropped.  But that
## implies a change in function definition, whch should be
## sufficient for a rebuild.
##
## TODO: An update -- because we're loading JSON we can't rely on map
## order.  Doing that with R is probably not safe anyway because these
## were sorted according to the current locale.  We'll need to
## establish a common ordering/name set for these.
compare_status <- function(prev, curr) {
  identical(prev, curr)
}

make_target <- function(name, dat, type=NULL) {
  if (name %in% target_reserved_names()) {
    stop(sprintf("Target name %s is reserved", name))
  }
  warn_unknown(name, dat,
               c("rule", "depends", "target_argument_name",
                 "cleanup_level", "quiet",
                 "chain",
                 # Special things
                 "plot"))
  chained <- !is.null(dat$chain)
  if (chained) {
    err <- intersect(c("depends", "rule", "target_argument_name"),
                     names(dat))
    if (length(err) > 0L) {
      stop(sprintf("Cannot specify %d when using chained rules",
                   paste(dQuote(err), collapse=" or ")))
    }
    dat$rule <- dat$chain
  }
  if (is.null(type)) {
    type <- if (target_is_file(name)) "file" else  "object"
    if (type == "object" && is.null(dat$rule) && is.null(dat$chain)) {
      type <- "fake"
    } else if (type == "file" && "plot" %in% names(dat)) {
      type <- "plot"
    }
  }
  type <- match_value(type, c("file", "plot", "object", "fake", "cleanup"))
  ## TODO: This would be better if we simply passed along "extra"
  ## perhaps, allowing fields in addition to
  ## rule/depends/cleanup_level to be checked?  But we ignore
  ## cleanup_level for fake and cleanup, so they're problematic too.
  if ("plot" %in% names(dat) && type != "plot") {
    stop("'plot' field invalid for targets of type ", type)
  }
  if ("target_argument_name" %in% names(dat) && type != "file") {
    stop("'target_argument_name' field invalid for arguments of type ", type)
  }
  rule <- dat$rule
  depends <- dat$depends
  cleanup_level <- with_default(dat$cleanup_level, "tidy")

  t <- switch(type,
              file=target_file$new(name, rule, depends, cleanup_level,
                dat$target_argument_name, chained),
              plot=target_plot$new(name, rule, depends, cleanup_level,
                dat$target_argument_name, chained, dat$plot),
              object=target_object$new(name, rule, depends, cleanup_level,
                chained),
              fake=target_fake$new(name, depends),
              cleanup=target_cleanup$new(name, rule, depends),
              stop("Unsupported type ", type))

  ## NOTE: This might not be the most sustainable way of doing this:
  if ("quiet" %in% names(dat)) {
    assert_scalar_logical(dat$quiet)
    t$quiet <- dat$quiet
  }

  t
}

target <- R6Class(
  "target",
  public=list(
    name=NULL,
    depends=NULL,
    rule=NULL,
    type=NULL,
    cleanup_level=NULL,
    store=NULL,
    chain=NULL,      # does this rule contain chained things
    chain_job=FALSE, # does this rule exist because of a chain?
    quiet=FALSE,

    initialize=function(name, rule, depends=NULL, cleanup_level="tidy",
      chained=FALSE) {
      #
      self$name <- name
      if (chained) {
        private$initialize_chain(rule, depends, cleanup_level)
      } else {
        private$initialize_single(rule, depends, cleanup_level)
      }
    },

    activate=function(maker) {
      self$store <- maker$store

      if (!is.null(self$chain)) {
        maker$add_targets(self$chain, activate=TRUE)
      }

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
      msg <- setdiff(depends_name, maker$target_names(all=TRUE))
      if (length(msg) > 0L) {
        if (!all(target_is_file(msg))) {
          stop("Implicitly created targets must all be files")
        }
        implicit <- lapply(msg, target_file$new, rule=NULL)
        maker$add_targets(implicit, activate=TRUE)
      }

      ## This preserves the original names:
      self$depends[] <- maker$get_targets(depends_name)
    },

    is_active=function() {
      !is.null(self$store)
    },

    get=function(fake=FALSE, for_script=FALSE) {
      stop("Not a target that can be got")
    },
    set=function(value) {
      stop("Not a target that can be got")
    },
    del=function(missing_ok=FALSE) {
      ## Or return()?
      stop("Not something that can be deleted")
    },

    is_current=function() {
      is_current(self, self$store)
    },

    status_string=function(current=NULL) {
      ""
    },

    dependencies=function() {
      sapply(self$depends, function(x) x$name)
    },

    ## Bad name, but the idea is simple: We want to return a list with
    ## only dependencies that are interesting (i.e, file/plot/object
    dependencies_real=function() {
      filter_targets_by_type(self$depends, c("file", "plot", "object"))
    },

    dependency_status=function(missing_ok=FALSE) {
      depends <- self$dependencies_real()
      names(depends) <- sapply(depends, function(x) x$name)
      depends <- lapply(depends, function(x) x$get_hash(missing_ok))
      list(version=self$store$version,
           name=self$name,
           depends=depends,
           code=self$store$env$deps$info(self$rule))
    },

    get_hash=function(missing_ok=FALSE) {
      self$store$get_hash(self$name, self$type, missing_ok)
    },

    dependencies_as_args=function(fake=FALSE, for_script=FALSE) {
      if (is.null(self$rule)) {
        list()
      } else {
        lapply(self$dependencies_real(), function(x) x$get(fake, for_script))
      }
    },

    run=function(quiet=FALSE) {
      if (is.null(self$rule)) {
        return()
      }
      args <- self$dependencies_as_args()

      ## Setting quiet in a target always overrides any runtime option.
      quiet <- quiet || self$quiet
      ## Suppressing cat() is hard:
      if (quiet) {
        temp <- file()
        sink(temp)
        on.exit(sink())
        on.exit(close(temp), add=TRUE)
      }
      ## NOTE: it's actually pretty easy here to print the output
      ## later if needed (e.g. if we catch errors in this bit).
      ## However it will not be possible to interleave the message
      ## stream and the output stream.
      withCallingHandlers(
        do.call(self$rule, args, envir=self$store$env$env),
        message=function(e) if (quiet) invokeRestart("muffleMessage"))
    },

    ## TODO: Merge into run?
    run_fake=function(for_script=FALSE) {
      if (is.null(self$rule)) {
        NULL
      } else {
        fake <- TRUE
        do_call_fake(self$rule, self$dependencies_as_args(fake, for_script))
      }
    },

    ## This method exists so that classes can define methods to run
    ## things before or after the run function.  See target_clean and
    ## target_file, which both do this.
    build=function(quiet=FALSE) {
      self$run(quiet=quiet)
    }
    ),
  private=list(
    initialize_single=function(rule, depends, cleanup_level) {
      if (!is.null(rule)) {
        assert_scalar_character(rule)
      }
      self$rule <- rule
      self$cleanup_level <- match_value(cleanup_level, cleanup_levels())
      ## These get wired up as actual maker::target objects on a
      ## second pass, but we need a full database to do that.
      self$depends <- from_yaml_map_list(depends)
    },

    initialize_chain=function(chain, depends, cleanup_level) {
      assert_list(chain)
      if (!is.null(names(chain))) {
        stop("Chained rules must be unnamed")
      }
      assert_null(depends)

      len <- length(chain)
      for (i in seq_len(len)) {
        x <- chain[[i]]
        dep <- c(if (i > 1L) chain[[i-1L]]$name, x$depends)
        cln <- with_default(x$cleanup_level, cleanup_level)
        if (i < len) {
          name <- chained_rule_name(self$name, i)
          ## TODO: More care will be needed for intermediates that
          ## aren't objects.  Going via files should actually be OK.
          t <- target_object$new(name, x$rule, dep, cln, FALSE)
          ## TODO: Better here would be have a target_object_chain and
          ## to link up this job as parent.
          ##
          ## TODO: Better than that would be to compose this, or to
          ## store the parent in chain_parent and test
          ## is.null(self$chain_parent)
          t$chain_job <- TRUE
          t$quiet <- self$quiet
          chain[[i]] <- t
        } else {
          private$initialize_single(x$rule, dep, cln)
        }
      }
      self$chain <- chain[-len]
    }
    ))

target_file <- R6Class(
  "target_file",
  inherit=target,
  public=list(
    ## Additional data field:
    target_argument_name=NULL,

    initialize=function(name, rule, depends=NULL, cleanup_level="tidy",
      target_argument_name=NULL, chained=FALSE) {
      if (is.null(rule)) {
        if (!missing(cleanup_level) && cleanup_level != "never") {
          stop("Don't do that")
        }
        cleanup_level <- "never"
      }
      super$initialize(name, rule, depends, cleanup_level)
      self$type <- "file"
      if (!is.null(target_argument_name) &&
          target_argument_name %in% names(depends)) {
        stop("target_argument_name clashes with named dependency")
      }
      self$target_argument_name <- target_argument_name
      if (is.null(rule) && !file.exists(name)) {
        warning("Creating implicit target for nonexistant file ", name)
      }
    },

    get=function(fake=FALSE, for_script=FALSE) {
      if (fake) {
        sprintf('"%s"', self$name)
      } else {
        self$name
      }
    },

    ## NOTE: this ignores the value.
    set=function(value) {
      self$store$db$set(self$name, self$dependency_status())
    },

    del=function(missing_ok=FALSE) {
      did_delete_obj <- self$store$files$del(self$name, missing_ok)
      did_delete_db  <- self$store$db$del(self$name, missing_ok)
      invisible(did_delete_obj || did_delete_db)
    },

    status_string=function(current=self$is_current()) {
      if (is.null(self$rule)) {
        ""
      } else if (current) {
        "OK"
      } else {
        "BUILD"
      }
    },

    dependencies_as_args=function(fake=FALSE, for_script=FALSE) {
      args <- super$dependencies_as_args(fake, for_script)
      if (!is.null(self$rule) && !is.null(self$target_argument_name)) {
        args[[self$target_argument_name]] <- self$get(fake, for_script)
      }
      args
    },

    build=function(quiet=FALSE) {
      ## These are implicit, and can't be built directly:
      if (is.null(self$rule)) {
        stop("Can't build implicit targets")
      }
      ## This avoids either manually creating directories, or obscure
      ## errors when R can't save a file to a place.  Possibly this
      ## should be a configurable behaviour, but we're guaranteed to
      ## be working with genuine files so this should be harmless.
      dir.create(dirname(self$name), showWarnings=FALSE, recursive=TRUE)
      res <- super$build()
      self$set(res)
      invisible(self$name)
    }
    ))

target_object <- R6Class(
  "target_object",
  inherit=target,
  public=list(
    initialize=function(name, rule, depends=NULL,
      cleanup_level="tidy", chained=FALSE) {
      if (is.null(rule)) {
        stop("Must not have a NULL rule")
      }
      super$initialize(name, rule, depends, cleanup_level, chained)
      self$type <- "object"
    },

    get=function(fake=FALSE, for_script=FALSE) {
      if (fake) {
        if (for_script && self$chain_job) {
          ## TODO: this will be heaps nicer if we store the true
          ## target name, perhaps as a reference?
          sub("\\{.+$", "", self$name)
        } else {
          self$name
        }
      } else {
        self$store$objects$get(self$name)
      }
    },

    set=function(value) {
      self$store$objects$set(self$name, value)
      self$store$db$set(self$name, self$dependency_status())
    },

    del=function(missing_ok=FALSE) {
      did_delete_obj <- self$store$objects$del(self$name, missing_ok)
      did_delete_db  <- self$store$db$del(self$name, missing_ok)
      invisible(did_delete_obj || did_delete_db)
    },

    status_string=function(current=self$is_current()) {
      if (current) "OK" else "BUILD"
    },

    build=function(quiet=FALSE) {
      res <- super$build(quiet=quiet)
      self$set(res)
      invisible(res)
    },

    run_fake=function(for_script=FALSE) {
      paste(self$get(TRUE, for_script), "<-", super$run_fake(for_script))
    }
    ))

target_cleanup <- R6Class(
  "target_cleanup",
  inherit=target,
  public=list(
    ## Special cleanup target needs a reference back to the original
    ## maker object, as we call back to that for the actual removal.
    maker=NULL,

    initialize=function(name, rule, depends=NULL) {
      super$initialize(name, rule, depends, "never")
      self$type <- "cleanup"
    },

    activate=function(maker) {
      super$activate(maker)
      self$maker <- maker
    },

    status_string=function(current) {
      "CLEAN"
    },

    build=function(quiet=FALSE) {
      self$maker$remove_targets(self$will_remove(), chain=FALSE)
      super$build(quiet=quiet) # runs any clean hooks
    },

    run_fake=function(for_script=FALSE) {
      NULL
    },

    will_remove=function() {
      target_level <- sapply(self$maker$targets, function(x) x$cleanup_level)
      names(self$maker$targets)[target_level == self$name]
    }
    ))

target_fake <- R6Class(
  "target_fake",
  inherit=target,
  public=list(
    initialize=function(name, depends=NULL) {
      super$initialize(name, NULL, depends, "never")
      self$type <- "fake"
    }
    ))

target_utility <- R6Class(
  "target_utility",
  inherit=target,
  public=list(
    utility=NULL,
    maker=NULL,

    initialize=function(name, utility, maker) {
      super$initialize(name, NULL, NULL, "never")
      self$type <- "utility"
      self$utility <- utility
      self$maker <- maker
    },

    run=function(quiet=FALSE) {
      self$utility(self$maker)
    },

    status_string=function(current=NULL) {
      "UTIL"
    }
    ))

target_plot <- R6Class(
  "target_plot",
  inherit=target_file,
  public=list(
    plot=NULL,

    initialize=function(name, rule, depends=NULL, cleanup_level="tidy",
      target_argument_name=NULL, chained=FALSE, plot=NULL) {
      if (is.null(rule)) {
        stop("Cannot have a NULL rule")
      }
      if (!is.null(target_argument_name)) {
        stop("Plot targets cannot use target_argument_name")
      }
      super$initialize(name, rule, depends, cleanup_level,
                       target_argument_name, chain)
      if (identical(plot, TRUE) || is.null(plot)) {
        plot <- list()
      }
      assert_list(plot)
      assert_named(plot)
      dev <- get_device(tools::file_ext(self$name))
      ## This will not work well for cases where `...` is in the
      ## device name (such as jpeg, bmp, etc)
      warn_unknown(paste0(name, ":plot"), plot, names(formals(dev)))
      self$plot <- list(device=dev, args=plot)
    },

    run=function(quiet=FALSE) {
      open_device(self$plot$device, self$plot_args(), self$store$env$env)
      on.exit(dev.off())
      super$run(quiet=quiet)
    },

    run_fake=function(for_script=FALSE) {
      cmd <- super$run_fake(for_script)
      if (for_script) {
        open <- do_call_fake(self$plot$device,
                             format_fake_args(self$plot_args(fake=TRUE)))
        c(open, cmd, "dev.off()")
      } else {
        paste(cmd, "# ==>", self$name)
      }
    },

    plot_args=function(fake=FALSE, for_script=FALSE) {
      ## TODO: If nonscalar arguments are passed into the plotting
      ## (not sure what takes them, but it's totally possible) then
      ## some care will be needed here.  It might be better to pick
      ## that up in `format_fake_args` though.
      c(list(self$get(fake, for_script)), self$plot$args)
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
    "md", "tex", "Rmd", "Rnw",
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
  if (target$type %in% c("cleanup", "fake", "utility")) {
    return(FALSE)
  } else if (!store$contains(target$name, target$type)) {
    return(FALSE)
  } else if (is.null(target$rule)) {
    return(TRUE)
  } else if (!store$db$contains(target$name)) {
    ## This happens when a file target exists, but there is no record
    ## of it being created (such as when the .maker directory is
    ## deleted or if it comes from elsewhere).  In which case we can't
    ## tell if it's up to date and assume not.
    return(FALSE)
  } else {
    ## TODO: This is all being done at once.  However, if targets
    ## offer a $compare_dependency_status() method, we can do this
    ## incrementally, returning FALSE as soon as the first failure is
    ## found.
    return(compare_dependency_status(
      store$db$get(target$name),
      target$dependency_status(missing_ok=TRUE)))
  }
}

compare_dependency_status <- function(prev, curr) {
  ## Here, if we need to deal with different version information we
  ## can.  One option will be to deprecate previous versions.  So say
  ## we change the format, or hash algorithms, or something and no
  ## longer allow version 0.1.  We'd say:
  ##
  ##   expire <- package_version("0.0")
  ##   if (prev$version <= expire) {
  ##     warning(sprintf("Expiring object %s (version: %s)",
  ##                     prev$name, prev$version))
  ##     return(FALSE)
  ##   }

  ## These make the comparisons insensitive to ordering.
  (identical_map(prev$depends, curr$depends) &&
   identical_map(prev$code,    curr$code))
}

## Not recursive:
identical_map <- function(x, y) {
  nms <- names(x)
  length(x) == length(y) && all(nms %in% names(y)) && identical(y[nms], x)
}

format_fake_args <- function(args) {
  nms <- names(args)
  args <- unlist(args)
  if (!is.null(nms)) {
    nms <- names(args)
    args <- ifelse(nms == "", args, paste(nms, args, sep="="))
  }
  paste(args, collapse=", ")
}

do_call_fake <- function(cmd, args) {
  assert_scalar_character(cmd)
  sprintf("%s(%s)", cmd, format_fake_args(args))
}

## There aren't many of these yet; might end up with more over time
## though.
target_reserved_names <- function() {
  c("deps", "gitignore")
}

filter_targets_by_type <- function(targets, types) {
  target_types <- sapply(targets, function(x) x$type)
  targets[target_types %in% types]
}

chained_rule_name <- function(name, i) {
  sprintf("%s{%d}", name, i)
}

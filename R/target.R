## TODO: Elsewhere run a tryCatch over this to uniformly add the
## target name to the error.
make_target <- function(name, dat, type=NULL) {
  if (name %in% target_reserved_names()) {
    stop(sprintf("Target name %s is reserved", name))
  }

  dat <- process_target_command(name, dat)

  if (is.null(type)) {
    type <- if (target_is_file(name)) "file" else  "object"
    if ("knitr" %in% names(dat$opts)) {
      type <- "knitr"
    } else if ("plot" %in% names(dat$opts)) {
      type <- "plot"
    } else if (type == "object" && is.null(dat$command$rule)) {
      type <- "fake"
    }
  }

  ## TODO: Get utility into this?  Possibly not as they're pretty
  ## different really having no command/deps
  generators <- list(object=target_object,
                     file=target_file,
                     plot=target_plot,
                     knitr=target_knitr,
                     fake=target_fake,
                     cleanup=target_cleanup)
  type <- match_value(type, names(generators))
  generators[[type]]$new(name, dat$command, dat$opts)
}

## Will change name soon, but the basic idea is to sort out what it is
## that we have to run:
##
## TODO: Need some tests here, throughout
process_target_command <- function(name, dat) {
  core <- c("command", "depends",
            "rule", "target_argument", "quoted", "depends_is_fake")

  ## Quick check that may disappear later:
  invalid <- c("rule", "target_argument", "quoted", "depends_is_fake")
  if (any(invalid %in% names(dat))) {
    stop("Invalid keys: ",
         paste(intersect(invalid, names(dat)), collapse=", "))
  }
  if (is.null(dat$command)) {
    dat$depends_is_fake <- rep(TRUE, length(dat$depends))
  } else {
    tmp <- parse_target_command(name, dat$command)

    tmp$depends_is_fake <-
      rep(c(FALSE, TRUE), c(length(tmp$depends), length(dat$depends)))
    if ("depends" %in% names(dat)) {
      tmp$depends <- c(tmp$depends, dat$depends)
    }
    dat[intersect(names(tmp), core)] <- tmp
  }
  is_command <- names(dat) %in% c(core)
  list(command=dat[is_command], opts=dat[!is_command])
}

target_base <- R6Class(
  "target_base",
  public=list(
    name=NULL,
    command=NULL,
    depends=NULL,
    ## Will change name soon:
    depends_is_fake=NULL,
    rule=NULL,
    type=NULL,
    cleanup_level="never",
    check="all",
    store=NULL,
    quiet=FALSE,

    initialize=function(name, command, opts, type="base") {
      self$name <- name
      ## TODO: This is a mess!  What we really have with the incoming
      ## 'command' is a processed set of commands.  We don't actually
      ## store this, but all the bits are scattered around.  'quoted'
      ## is used only during validation.
      self$command <- command$command
      private$quoted <- command$quoted

      opts <- self$check_opts(opts)

      if (!is.null(command$rule)) {
        assert_scalar_character(command$rule,
                                paste(name, "rule", sep=": "))
      }
      self$rule <- command$rule

      self$depends <- from_yaml_map_list(command$depends)
      sapply(self$depends, assert_scalar_character)
      self$depends_is_fake <- command$depends_is_fake

      if ("target_argument" %in% names(command) && type != "file") {
        stop("'target_argument' field invalid for arguments of type ", type)
      }

      if (!is.null(opts$cleanup_level)) {
        self$cleanup_level <-
          match_value(opts$cleanup_level, cleanup_levels(),
                      paste(name, "cleanup_level", sep=": "))
      }

      assert_scalar_character(type)
      self$type <- type

      if ("quiet" %in% names(opts)) {
        assert_scalar_logical(opts$quiet, paste(name, "quiet", sep=": "))
        self$quiet <- opts$quiet
        if (is.null(self$rule)) {
          warning("Using 'quiet' on a rule-less target has no effect")
        }
      }

      if ("check" %in% names(opts)) {
        self$check <- match_value(opts$check, check_levels(),
                                  paste(name, "quiet", sep=": "))
        if (is.null(self$rule)) {
          warning("Using 'check' on a rule-less target has no effect")
        }
      }
    },

    valid_options=function() {
      c("quiet", "check")
    },

    check_opts=function(opts) {
      err <- setdiff(names(opts), self$valid_options())
      if (length(err) > 0) {
        stop(sprintf("Invalid options for %s: %s",
                     self$name, paste(err, collapse=", ")))
      }
      opts
    },

    activate=function(maker) {
      self$store <- maker$store
      if (length(self$depends) == 0L) {
        return()
      }

      ## TODO: Need to get some good testing in here:
      depends_name <- sapply(self$depends, "[[", 1)
      if (any(duplicated(depends_name))) {
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
        implicit <- lapply(msg, target_file$new, NULL, NULL)
        maker$add_targets(implicit, activate=TRUE)
      }

      ## This preserves the original names:
      self$depends[] <- maker$get_targets(depends_name)
      private$check_quoted()
    },

    is_active=function() {
      !is.null(self$store)
    },

    ## These basically prevent using target_base
    get=function(fake=FALSE, for_script=FALSE) {
      stop("Not something that can be got")
    },
    set=function(value) {
      stop("Not something that can be set")
    },
    del=function(missing_ok=FALSE) {
      stop("Not something that can be deleted")
    },
    archive_export=function(path, missing_ok=FALSE) {
      stop("Not something that can be copied")
    },
    archive_import=function(path) {
      stop("Not something that can be imported")
    },

    is_current=function(check=NULL) {
      is_current(self, self$store, check)
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
      filter_targets_by_type(self$depends,
                             c("file", "plot", "object"))
    },

    dependency_status=function(missing_ok=FALSE, check=NULL) {
      check <- with_default(check, self$check)
      depends <- code <- NULL

      if (check_depends(check)) {
        depends <- self$dependencies_real()
        names(depends) <- sapply(depends, function(x) x$name)
        depends <- lapply(depends, function(x) x$get_hash(missing_ok))
      }

      if (check_code(check)) {
        code <- self$store$env$deps$info(self$rule)
      }

      list(version=self$store$version,
           name=self$name,
           depends=depends,
           code=code)
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

    run=function(quiet=NULL) {
      if (is.null(self$rule)) {
        return()
      }
      args <- self$dependencies_as_args()

      ## Setting quiet in a target always overrides any runtime
      ## option.
      ## TODO: quiet is not getting sanitised here.  Run via isTRUE?
      quiet <- with_default(quiet, self$quiet)
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

    ## TODO: Compute on initialise
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
    build=function(quiet=NULL) {
      self$run(quiet=quiet)
    }
    ),
  private=list(
    quoted=NULL,

    ## This whole section is a bit silly, but will save some
    ## confusion down the track.  Basically; file targets must be
    ## quoted, object targets must not be.  This lets us mimic R
    ## calls.  It's not actually required by any of the parsing
    ## machinery, but it means the files will be easier to
    ## interpret.

    check_quoted=function() {
      quoted <- private$quoted
      if (!is.null(quoted)) {
        i <- !self$depends_is_fake
        depends_name <- sapply(self$depends[i], function(x) x$name)
        depends_type <- sapply(self$depends[i], function(x) x$type)
        assert_length(quoted, length(depends_name))
        should_be_quoted <- depends_type == "file"
        if (any(should_be_quoted != quoted)) {
          err_quote <- depends_name[should_be_quoted  & !quoted]
          err_plain <- depends_name[!should_be_quoted &  quoted]
          msg <- character(0)
          if (length(err_quote) > 0) {
            msg <- c(msg, paste("Should be quoted:",
                                paste(err_quote, collapse=", ")))
          }
          if (length(err_plain) > 0) {
            msg <- c(msg, paste("Should not be quoted:",
                                paste(err_plain, collapse=", ")))
          }
          stop(sprintf("Incorrect quotation in target '%s':\n%s",
                       self$name, paste(msg, collapse="\n")))
        }
      }
    }
    ))


target_file <- R6Class(
  "target_file",
  inherit=target_base,
  public=list(
    ## Additional data field:
    target_argument=NULL,

    initialize=function(name, command, opts) {
      if (is.null(command$rule)) {
        ## NOTE: If the a rule is null (probably an implicit file)
        ## then we should never clean it up.  It's not clear that this
        ## should necessarily be an error but that will avoid
        ## accidentally deleting something important.
        opts$cleanup_level <- with_default(opts$cleanup_level, "never")
        if (opts$cleanup_level != "never") {
          stop("Probably unsafe to delete files we can't recreate")
        }
        if (!file.exists(name)) {
          warning("Creating implicit target for nonexistant file ", name)
        }
      } else {
        opts$cleanup_level <- with_default(opts$cleanup_level, "clean")
      }
      super$initialize(name, command, opts, "file")
      self$target_argument <- command$target_argument
    },

    valid_options=function() {
      c(super$valid_options(), "cleanup_level")
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
      self$store$db$set(self$name, self$dependency_status(check="all"))
    },

    del=function(missing_ok=FALSE) {
      did_delete_obj <- self$store$files$del(self$name, missing_ok)
      did_delete_db  <- self$store$db$del(self$name, missing_ok)
      invisible(did_delete_obj || did_delete_db)
    },

    archive_export=function(path, missing_ok=FALSE, missing_ok_db=missing_ok) {
      assert_directory(path)
      path_files <- file.path(path, "files")
      path_db <- file.path(path, "db")
      dir.create(path_files, FALSE)
      dir.create(path_db, FALSE)
      self$store$files$archive_export(self$name, path_files, missing_ok)
      self$store$db$archive_export(self$name, path_db, missing_ok_db)
    },

    archive_import=function(path) {
      path_files <- file.path(path, "files")
      path_db <- file.path(path, "db")
      self$store$db$archive_import(self$name, path_db)
      self$store$files$archive_import(self$name, path_files)
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
      if (!is.null(self$rule) && !is.null(self$target_argument)) {
        if (is.character(self$target_argument)) {
          val <- self$get(fake, for_script)
          args[[self$target_argument]] <- self$get(fake, for_script)
        } else {
          args <- insert_at(args, self$get(fake, for_script),
                            self$target_argument)
        }
      }
      args
    },

    build=function(quiet=NULL) {
      ## These are implicit, and can't be built directly:
      if (is.null(self$rule)) {
        stop("Can't build implicit targets")
      }
      ## This avoids either manually creating directories, or obscure
      ## errors when R can't save a file to a place.  Possibly this
      ## should be a configurable behaviour, but we're guaranteed to
      ## be working with genuine files so this should be harmless.
      dir.create(dirname(self$name), showWarnings=FALSE, recursive=TRUE)

      ## NOTE: I'm using withCallingHandlers here because that does
      ## allow options(error=recover) to behave in the expected way
      ## (i.e., the target function remains on the stack and can be
      ## inspected/browsed).
      path <- self$backup()
      withCallingHandlers(super$build(),
                          error=function(e) {
                            self$restore(path)
                            stop(e)
                          })
      ## This only happens if the error is not raised above:
      self$set(res)
      invisible(self$name)
    },

    ## Used in build/restore
    backup=function() {
      if (file.exists(self$name)) {
        path <- file.path(tempfile(), self$name)
        dir.create(dirname(path), showWarnings=FALSE, recursive=TRUE)
        file.copy(self$name, path)
        path
      } else {
        NULL
      }
    },
    restore=function(path) {
      if (!is.null(path)) {
        message("Restoring previous version of ", self$name)
        file.copy(path, self$name, overwrite=TRUE)
      }
    }
    ))

target_object <- R6Class(
  "target_object",
  inherit=target_base,
  public=list(
    initialize=function(name, command, opts=NULL) {
      if (is.null(command$rule)) {
        stop("Must not have a NULL rule")
      }
      opts$cleanup_level <- with_default(opts$cleanup_level, "tidy")
      super$initialize(name, command, opts, "object")
    },

    valid_options=function() {
      c(super$valid_options(), "cleanup_level")
    },

    get=function(fake=FALSE, for_script=FALSE) {
      if (fake) {
        self$name
      } else {
        self$store$objects$get(self$name)
      }
    },

    ## NOTE: When creating the entry in the database, we add *all* of
    ## the dependency information, even if this target won't use it
    ## all.  This is possibly slower than ideal, especially for things
    ## that depend on very large files.
    set=function(value) {
      self$store$objects$set(self$name, value)
      self$store$db$set(self$name, self$dependency_status(check="all"))
    },

    del=function(missing_ok=FALSE) {
      did_delete_obj <- self$store$objects$del(self$name, missing_ok)
      did_delete_db  <- self$store$db$del(self$name, missing_ok)
      invisible(did_delete_obj || did_delete_db)
    },

    archive_export=function(path, missing_ok=FALSE, missing_ok_db=missing_ok) {
      assert_directory(path)
      path_objects <- file.path(path, "objects")
      path_db <- file.path(path, "db")
      dir.create(path_objects, FALSE)
      dir.create(path_db, FALSE)
      self$store$objects$archive_export(self$name, path_objects, missing_ok)
      self$store$db$archive_export(self$name, path_db, missing_ok_db)
    },

    archive_import=function(path) {
      assert_directory(path)
      path_objects <- file.path(path, "objects")
      path_db <- file.path(path, "db")
      self$store$objects$archive_import(self$name, path_objects)
      self$store$db$archive_import(self$name, path_db)
    },

    status_string=function(current=self$is_current()) {
      if (current) "OK" else "BUILD"
    },

    build=function(quiet=NULL) {
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
  inherit=target_base,
  public=list(
    ## Special cleanup target needs a reference back to the original
    ## maker object, as we call back to that for the actual removal.
    maker=NULL,

    initialize=function(name, command, opts) {
      super$initialize(name, command, opts, "cleanup")
    },

    activate=function(maker) {
      super$activate(maker)
      self$maker <- maker
    },

    status_string=function(current) {
      "CLEAN"
    },

    build=function(quiet=NULL) {
      self$maker$remove_targets(self$will_remove())
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
  inherit=target_base,
  public=list(
    initialize=function(name, command, opts) {
      if (!is.null(command$rule)) {
        stop("fake targets must have a NULL rule (how did you do this?)")
      }
      super$initialize(name, command, opts, "fake")
    }
    ))

## Note that this is *totally* different to the above, at least for
## now.
target_utility <- R6Class(
  "target_utility",
  inherit=target_base,
  public=list(
    utility=NULL,
    maker=NULL,

    initialize=function(name, utility, maker) {
      super$initialize(name, NULL, NULL, "utility")
      self$utility <- utility
      self$maker <- maker
    },

    run=function(quiet=NULL) {
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

    initialize=function(name, command, opts) {
      if (is.null(command$rule)) {
        stop("Cannot have a NULL rule")
      }
      super$initialize(name, command, opts)
      self$plot <- opts$plot # checked at activate()
    },

    valid_options=function() {
      c(super$valid_options(), "plot")
    },

    activate=function(maker) {
      super$activate(maker)
      dev <- get_device(tools::file_ext(self$name))

      plot <- self$plot
      if (identical(plot, TRUE) || is.null(plot)) {
        plot <- list()
      } else if (is.character(plot) && length(plot) == 1) {
        if (plot %in% names(maker$plot_options)) {
          plot <- maker$plot_options[[plot]]
        } else {
          stop(sprintf("Unknown plot_options '%s' in target '%s'",
                       plot, self$name))
        }
      } else {
        assert_list(plot)
        assert_named(plot)
      }
      ## This will not work well for cases where `...` is in the
      ## device name (such as jpeg, bmp, etc)
      warn_unknown(paste0(name, ":plot"), plot, names(formals(dev)))
      self$plot <- list(device=dev, args=plot)
    },

    run=function(quiet=NULL) {
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

target_knitr <- R6Class(
  "target_knitr",
  inherit=target_file,
  public=list(
    knitr=NULL,
    maker=NULL,

    ## Ideas here:
    ##  - export_all: export all objects in the store?
    ##  - export_source: export the source functions (or rather, don't)
    ##  - set knitr options as a hook on run?
    ##  - render to html, etc.
    initialize=function(name, command, opts) {
      if (!is.null(command$rule)) {
        stop(sprintf("%s: knitr targets must have a NULL rule",
                     name))
      }

      opts$quiet <- with_default(opts$quiet, TRUE)

      opts$auto_figure_prefix <-
        with_default(opts$auto_figure_prefix, FALSE)
      assert_scalar_logical(opts$auto_figure_prefix)

      ## Then the knitr options:
      knitr <- opts$knitr
      if (identical(knitr, TRUE) || is.null(knitr)) {
        knitr <- list()
      }
      warn_unknown(paste(name, "knitr", sep=": "), knitr,
                   c("input", "options", "auto_figure_prefix"))

      ## Infer name if it's not present:
      if (is.null(knitr$input)) {
        knitr$input <- knitr_infer_source(name)
      }
      assert_scalar_character(knitr$input)

      ## NOTE: It might be useful to set fig.path here, so that we can
      ## work out what figures belong with different knitr targets.
      ## What I'm going to do though is *not* do that at the moment
      ## though.  Better would be to have a key (e.g.,
      ## fig.path.disambiguate) that indicate that the prefix should
      ## be set using the fig_default_fig_path function.  Then the
      ## default gets the same behaviour as default knitr.
      if (is.null(knitr$options)) {
        knitr$options <- list()
      }
      if (opts$auto_figure_prefix) {
        if (is.null(knitr$options$fig.path)) {
          knitr$options$fig.path <- knitr_default_fig_path(name)
        } else {
          warning("Ignoring 'auto_figure_prefix' in favour of 'fig.path'")
        }
      }

      ## Build a dependency on the input, for obvious reasons:
      command$depends <- c(command$depends, list(knitr$input))
      command$depends_is_fake <- c(command$depends_is_fake, TRUE)

      ## Hack to let target_base know we're not implicit.  There does
      ## need to be something here as a few places test for null-ness.
      command$rule <- ".__knitr__"
      super$initialize(name, command, opts)
      self$knitr <- knitr
    },

    activate=function(maker) {
      super$activate(maker)
      self$maker <- maker
    },

    valid_options=function() {
      c(super$valid_options(), "knitr", "auto_figure_prefix")
    },

    status_string=function(current=self$is_current()) {
      if (current) "OK" else "KNIT"
    },

    run=function(quiet=NULL) {
      object_names <- knitr_depends(self$maker, self$depends)
      knitr_from_maker(self$knitr$input, self$name, self$store,
                       object_names,
                       quiet=with_default(quiet, self$quiet),
                       knitr_options=self$knitr$options)
    },

    run_fake=function(for_script=FALSE) {
      sprintf('knitr::knit("%s", "%s")', self$knitr$input, self$name)
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
is_current <- function(target, store, check=NULL) {
  check <- with_default(check, target$check)
  check <- match_value(check, check_levels())

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
    ##
    ## *However* if check is 'exists', then this is enough because we
    ## don't care about the code or the dependencies.
    return(check == "exists")
  } else {
    ## TODO: This is all being done at once.  However, if targets
    ## offer a $compare_dependency_status() method, we can do this
    ## incrementally, returning FALSE as soon as the first failure is
    ## found.
    ##
    ## TODO: Need options for deciding what to check (existance, data,
    ## code).
    return(compare_dependency_status(
      store$db$get(target$name),
      target$dependency_status(missing_ok=TRUE, check=check),
      check))
  }
}

compare_dependency_status <- function(prev, curr, check) {
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
  ## TODO: This check is not actually needed here.
  check <- match_value(check, check_levels())
  ok <- TRUE

  if (check_depends(check)) {
    ok <- ok && identical_map(prev$depends, curr$depends)
  }
  if (check_code(check)) {
    ## TODO: I've dropped checking *packages* here: see #13
    ok <- ok && identical_map(prev$code$functions, curr$code$functions)
  }

  ok
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
  c("install_packages", "gitignore", "target_name", ".")
}

filter_targets_by_type <- function(targets, types) {
  target_types <- sapply(targets, function(x) x$type)
  targets[target_types %in% types]
}

chained_rule_name <- function(name, i) {
  sprintf("%s{%d}", name, i)
}

check_levels <- function() {
  c("all", "code", "depends", "exists")
}

check_code <- function(x) {
  x %in% c("all", "code")
}
check_depends <- function(x) {
  x %in% c("all", "depends")
}

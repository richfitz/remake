target_infer_type <- function(name, type, dat) {
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
  type
}

## TODO: Elsewhere run a tryCatch over this to uniformly add the
## target name to the error.
make_target <- function(name, dat) {
  assert_scalar_character(name)
  if (name %in% target_reserved_names()) {
    stop(sprintf("Target name %s is reserved", name))
  }

  ## This is just a wrapper function to improve the traceback on error.
  make_target_dat <- function(dat) {
    assert_named_list(dat, name="target data")

    ## TODO: process type within process_target_command, I think.
    type <- dat$type
    if (!is.null(type)) {
      assert_scalar_character(type)
      dat <- dat[names(dat) != "type"]
    }

    dat <- process_target_command(name, dat)
    type <- target_infer_type(name, type, dat)

    ## TODO: Get utility into this?  Possibly not as they're pretty
    ## different really having no command/deps
    generators <- list(object=target_new_object,
                       file=target_new_file,
                       plot=target_new_plot,
                       knitr=target_new_knitr,
                       fake=target_new_fake,
                       cleanup=target_new_cleanup)
    type <- match_value(type, names(generators))
    generators[[type]](name, dat$command, dat$opts)
  }

  prefix <- sprintf("While processing target '%s':\n    ", name)
  withCallingHandlers(make_target_dat(dat),
                      error=catch_error_prefix(prefix),
                      warning=catch_warning_prefix(prefix))
}

target_default_cleanup <- function(type) {
  switch(type,
         file="clean",
         object="tidy",
         "never")
}

target_new_base <- function(name, command, opts, type="base",
                            valid_options=NULL) {
  assert_scalar_character(name)
  assert_scalar_character(type)
  if ("target_argument" %in% names(command) && type != "file") {
    stop("'target_argument' field invalid for arguments of type ", type)
  }

  ret <- list(name=name, type=type)
  ret$command <- command$command
  ret$quoted <- command$quoted

  ## TODO: Could do this with warn_unknown, or implement stop_unknown?
  ## TODO: This is really ugly - would be easier if we passed in the
  ## correct type at the beginning, and then we can use
  ## target_valid_options.
  valid_options <- c("quiet", "check", "packages", valid_options)
  err <- setdiff(names(opts), valid_options)
  if (length(err) > 0) {
    stop(sprintf("Invalid options for %s: %s",
                 name, paste(err, collapse=", ")))
  }

  if (!is.null(command$rule)) {
    assert_scalar_character(command$rule, "rule")
    ret$rule <- command$rule
  }

  ## TODO: Do all this checking during the process command section.
  ## No need for this to clutter this up, as it applies everywhere.
  if (length(command$depends) > 0) {
    depends <- from_yaml_map_list(command$depends)
    sapply(depends, assert_scalar_character)
    ret$depends <- unlist(depends)
  } else {
    ret$depends <- list()
  }
  if (any(duplicated(ret$depends))) {
    stop("Dependency listed more than once")
  }
  if (any(duplicated(setdiff(names(ret$depends), "")))) {
    stop("All named depends targets must be unique")
  }

  assert_length(command$depends_is_arg, length(command$depends))
  if (length(command$depends_is_arg) > 0L) {
    assert_logical(command$depends_is_arg)
  }
  ret$depends_is_arg <- command$depends_is_arg

  ## Use with_default here?
  if (is.null(opts$cleanup_level)) {
    ret$cleanup_level <- "never"
  } else {
    ret$cleanup_level <-
      match_value(opts$cleanup_level, cleanup_levels(), "cleanup_level")
  }

  ## TODO: with_default?
  if ("quiet" %in% names(opts)) {
    assert_scalar_logical(opts$quiet, "quiet")
    ret$quiet <- opts$quiet
    if (is.null(ret$rule)) {
      warning("Using 'quiet' on a rule-less target has no effect")
    }
  } else {
    ret$quiet <- FALSE
  }

  ## TODO: with_default?
  if ("check" %in% names(opts)) {
    ret$check <- match_value(opts$check, check_levels(), "check")
    if (is.null(ret$rule)) {
      warning("Using 'check' on a rule-less target has no effect")
    }
  } else {
    ret$check <- "all"
  }

  ret$status_string <- ""

  if ("packages" %in% names(opts)) {
    ret$packages <- opts$packages
    assert_character(opts$packages)
  }

  if ("chain" %in% names(command)) {
    chain <- target_chain(command$chain, ret, opts)
    ret <- chain$parent
    ret$chain_kids <- chain$kids
  }

  class(ret) <- "target_base"
  ret
}

target_new_object <- function(name, command, opts, valid_options=NULL) {
  if (is.null(command$rule)) {
    stop("Must not have a NULL rule")
  }
  opts$cleanup_level <- with_default(opts$cleanup_level, "tidy")
  valid_options <- c("cleanup_level", valid_options)
  ret <- target_new_base(name, command, opts, "object", valid_options)
  ret$status_string <- "BUILD"
  class(ret) <- c("target_object", class(ret))
  ret
}

target_new_file <- function(name, command, opts, valid_options=NULL) {
  if (is.null(command$rule)) {
    stop("Must not have a NULL rule")
  }
  opts$cleanup_level <- with_default(opts$cleanup_level, "clean")
  valid_options <- c("cleanup_level", valid_options)
  ret <- target_new_base(name, command, opts, "file", valid_options)
  ret$target_argument <- command$target_argument
  ret$status_string <- "BUILD"
  class(ret) <- c("target_file", class(ret))
  ret
}

## This is called directly by maker, and skips going through
## target_new.  That will probably change back shortly.
target_new_file_implicit <- function(name) {
  if (!file.exists(name)) {
    warning("Creating implicit target for nonexistant file ", name)
  }
  ret <- list(name=name,
              type="file",
              depends=list(),
              implicit=TRUE,
              check="exists")
  class(ret) <- c("target_file_implicit", "target_file") # not target_base
  ret
}

target_new_plot <- function(name, command, opts) {
  if (is.null(command$rule)) {
    stop("Cannot have a NULL rule")
  }
  ret <- target_new_file(name, command, opts, "plot")
  ret$plot <- opts$plot # checked at activate()
  class(ret) <- c("target_plot", class(ret))
  ret
}

target_new_knitr <- function(name, command, opts) {
  if (!is.null(command$rule)) {
    stop(sprintf("%s: knitr targets must have a NULL rule",
                 name))
  }

  opts$quiet <- with_default(opts$quiet, TRUE)

  ## Then the knitr options:
  knitr <- opts$knitr
  if (identical(knitr, TRUE) || is.null(knitr)) {
    knitr <- list()
  }
  warn_unknown("knitr", knitr, c("input", "options", "chdir",
                                 "auto_figure_prefix"))

  ## Infer name if it's not present:
  if (is.null(knitr$input)) {
    knitr$input <- knitr_infer_source(name)
  }
  assert_scalar_character(knitr$input)

  knitr$auto_figure_prefix <-
    with_default(knitr$auto_figure_prefix, FALSE)
  assert_scalar_logical(knitr$auto_figure_prefix)

  knitr$chdir <- with_default(knitr$chdir, FALSE)
  assert_scalar_logical(knitr$chdir)

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
  if (knitr$auto_figure_prefix && !is.null(knitr$options$fig.path)) {
    warning("Ignoring 'auto_figure_prefix' in favour of 'fig.path'")
    knitr$auto_figure_prefix <- FALSE
  }
  ## By default we *will* set error=FALSE.  It's hard to imagine a
  ## workflow where that is not what is wanted.  Better might be
  ## to allow the compilation to continue but detect if there was
  ## an error and throw an error at the target level though.
  if (is.null(knitr$options$error)) {
    knitr$options$error <- FALSE
  }

  ## Build a dependency on the input, for obvious reasons:
  command$depends <- c(command$depends, list(knitr$input))
  ## This dependency is not an argument
  command$depends_is_arg <- c(command$depends_is_arg, FALSE)

  ## Hack to let target_base know we're not implicit.  There does
  ## need to be something here as a few places test for null-ness.
  command$rule <- ".__knitr__"
  ret <- target_new_file(name, command, opts, "knitr")
  class(ret) <- c("target_knitr", class(ret))
  ret$knitr <- knitr
  ret
}

target_new_cleanup <- function(name, command, opts) {
  ret <- target_new_base(name, command, opts, "cleanup")
  ret$status_string <- "CLEAN"
  class(ret) <- c("target_cleanup", class(ret))
  ret
}

target_new_fake <- function(name, command, opts) {
  if (!is.null(command$rule)) {
    stop("fake targets must have a NULL rule (how did you do this?)")
  }
  ret <- target_new_base(name, command, opts, "fake")
  class(ret) <- c("target_fake", class(ret))
  ret
}

target_new_utility <- function(name, utility, maker) {
  ret <- target_new_base(name, NULL, NULL, "utility")
  ret$utility <- utility
  ret$maker <- maker
  ret$status_string <- "UTIL"
  class(ret) <- c("target_utility", class(ret))
  ret
}

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
    "md", "tex", "R", "Rmd", "Rnw", "html", "htm", "bib",
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
    ## TODO: This is all being done at once.  However, if we implement
    ## a compare_dependency_status() function, we can do this
    ## incrementally, returning FALSE as soon as the first failure is
    ## found.
    ##
    ## TODO: Need options for deciding what to check (existance, data,
    ## code).
    return(compare_dependency_status(
      store$db$get(target$name),
      dependency_status(target, store, missing_ok=TRUE, check=check),
      check))
  }
}

dependency_status <- function(target, store, missing_ok=FALSE, check=NULL) {
  check <- with_default(check, target$check)
  depends <- code <- NULL

  if (check_depends(check)) {
    depends_type <- target$depends_type
    depends_name <- target$depends
    keep <- depends_type %in% c("file", "object")
    depends <- lapply(which(keep), function(i)
                      store$get_hash(depends_name[[i]],
                                     depends_type[[i]], missing_ok))
    names(depends) <- depends_name[keep]
  }

  if (check_code(check)) {
    code <- store$env$deps$info(target$rule)
  }

  list(version=store$version,
       name=target$name,
       depends=depends,
       code=code)
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
  target_types <- dependency_types(targets)
  targets[target_types %in% types]
}

dependency_names <- function(x) {
  if (length(x) > 0) {
    sapply(x, function(el) el$name)
  } else {
    character(0)
  }
}
dependency_types <- function(x) {
  if (length(x) > 0) {
    sapply(x, function(el) el$type)
  } else {
    character(0)
  }
}

## TODO: There is an issue here for getting options for rules that
## terminate in knitr or plot rules: we can't pass along options to
## these!
##
##   Always accept quiet, check, packages (base)
##     cleanup_level (file and object)
##   never plot, knitr, auto_figure_prefix
##
## Special testing will be required to get that right.  Basically only
## the terminating bit of rule here will accept nonstandard options.
target_chain <- function(chain, parent, opts) {
  if (!(parent$type %in% c("file", "object"))) {
    stop("Can't use chained rules on targets of type ", parent)
  }
  len <- length(chain)
  chain_names <- chained_rule_name(parent$name, seq_len(len))
  parent <- target_chain_match_dot(parent, len + 1L, chain_names)

  ## TODO: Duplication of object valid options here.
  opts_chain <- opts[names(opts) %in%
                     c("quiet", "check", "packages", "cleanup_level")]
  f <- function(i) {
    x <- target_new_object(chain_names[[i]], chain[[i]], opts_chain)
    x$chain_parent <- parent
    target_chain_match_dot(x, i, chain_names)
  }

  kids <- lapply(seq_len(len), f)
  list(parent=parent, kids=kids)
}

target_chain_match_dot <- function(x, pos, chain_names) {
  j <- which(as.character(x$depends) == ".")
  if (length(j) == 1L) {
    if (j > length(chain_names)) {
      stop("Attempt to select impossible chain element") # defensive only
    }
    x$depends[[j]] <- chain_names[[pos - 1L]]
  } else { # defensive - should be safe here.
    if (length(j) > 1L) {
      stop("never ok")
    } else if (pos > 1L) {
      stop("missing")
    }
  }
  x
}

make_target_cleanup <- function(name, maker) {
  levels <- cleanup_target_names()
  name <- match_value(name, levels)
  i <- match(name, levels)
  if (name %in% maker$target_names()) {
    t <- maker$get_target(name)

    ## These aren't tested:
    if (!is.null(t$chain_kids)) {
      stop("Cleanup target cannot contain a chain")
    }
    if (length(t$depends) > 0L) {
      ## This is far from pretty.  Basically I need to know whether
      ## the dependencies we have been given here came from a depends:
      ## entry or from arguments to the command.  The former is fine,
      ## the latter is not.  Previously this was what the
      ## depends_is_fake argument was for.  The simplest way here is
      ## just to parse the damn thing again:
      if (length(parse_command(t$command)$depends) > 0L) {
        stop("Cleanup target commands must have no arguments")
      }
    }
    command <- t$command
    depends <- t$depends

    ## TODO: Ideally we'd also get quiet from the target here, too.
  } else {
    command <- depends <- NULL
  }
  if (i > 1L) {
    depends <- c(depends, list(levels[[i - 1L]]))
  }
  make_target(name, list(command=command, depends=depends, type="cleanup"))
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

target_get <- function(target, store, fake=FALSE) {
  if (target$type == "file") {
    if (fake) {
      sprintf('"%s"', target$name)
    } else {
      target$name
    }
  } else if (target$type == "object") {
    if (fake) {
      target$name
    } else {
      store$objects$get(target$name)
    }
  } else {
    stop("Not something that can be got")
  }
}
target_set <- function(target, store, value) {
  if (target$type == "file") {
    ## NOTE: value ignored here, will be NULL probably.
    store$db$set(target$name,
                 dependency_status(target, store, check="all"))
  } else if (target$type == "object") {
    store$objects$set(target$name, value)
    store$db$set(target$name,
                 dependency_status(target, store, check="all"))
  } else {
    stop("Not something that can be set")
  }
}

## This whole section is a bit silly, but will save some confusion
## down the track.  Basically; file targets must be quoted, object
## targets must not be.  This lets us mimic R calls.  It's not
## actually required by any of the parsing machinery, but it means the
## files will be easier to interpret.
target_check_quoted <- function(target) {
  quoted <- target$quoted
  if (!is.null(quoted) && length(quoted) > 0L) {
    i <- target$depends_is_arg
    depends_name <- target$depends[i]
    depends_type <- target$depends_type[i]
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
                   target$name, paste(msg, collapse="\n")))
    }
  }
}

dependencies_as_args <- function(target, store, fake=FALSE) {
  if (is.null(target$rule)) {
    list()
  } else {
    deps_name <- target$depends[target$depends_is_arg]
    deps_type <- target$depends_type[target$depends_is_arg]
    keep <- deps_type %in% c("file", "object")

    ## TODO: A hack for now.
    deps <- lapply(which(keep), function(i)
                   list(name=deps_name[[i]],
                        type=deps_type[[i]]))
    names(deps) <- names(target$depends)[keep]

    args <- lapply(deps, function(x) target_get(x, store, fake))
    if (!is.null(target$target_argument)) {
      val <- target_get(target, store, fake)
      if (is.character(target$target_argument)) {
        args[[target$target_argument]] <- val
      } else {
        args <- insert_at(args, val, target$target_argument)
      }
    }
    args
  }
}

plot_args <- function(target, fake=FALSE) {
  str <- if (fake) sprintf('"%s"', target$name) else target$name
  c(str, target$plot$args)
}

## target_valid_options <- function(type) {
##   base <- c("quiet", "check", "packages")
##   if (type == "file") {
##     valid <- c(base, "cleanup_level")
##   } else if (type == "object") {
##     valid <- c(base, "cleanup_level")
##   } else if (type == "plot") {
##     valid <- c(base, "cleanup_level", "plot")
##   } else if (type == "knitr") {
##     valid <- c(base, "cleanup_level", "knitr")
##   } else {
##     valid <- base
##   }
##   valid
## }

## Might compute these things at startup, given they are constants
## over the life of the object.
target_run_fake <- function(target, for_script=FALSE) {
  if (is.null(target$rule) || target$type == "cleanup") {
    NULL
  } else {
    res <- do_call_fake(target$rule,
                        dependencies_as_args(target, NULL, TRUE))
    if (inherits(target, "target_plot")) {
      if (for_script) {
        open <- do_call_fake(target$plot$device,
                             format_fake_args(plot_args(target, fake=TRUE)))
        res <- c(open, res, "dev.off()")
      } else {
        res <- paste(res, "# ==>", target$name)
      }
    } else if (inherits(target, "target_knitr")) {
      res <- sprintf('knitr::knit("%s", "%s")',
                     target$knitr$input, target$name)
    } else if (target$type == "object") {
      res <- paste(target$name, "<-", res)
    }
    res
  }
}

target_build <- function(target, store, quiet=NULL) {
  if (target$type == "file") {
    if (is.null(target$rule)) {
      ## NOTE: Not sure this is desirable - should just pass?
      stop("Can't build implicit targets")
    }
    ## This avoids either manually creating directories, or obscure
    ## errors when R can't save a file to a place.  Possibly this
    ## should be a configurable behaviour, but we're guaranteed to
    ## be working with genuine files so this should be harmless.
    dir.create(dirname(target$name), showWarnings=FALSE, recursive=TRUE)
    ## NOTE: I'm using withCallingHandlers here because that does
    ## allow options(error=recover) to behave in the expected way
    ## (i.e., the target function remains on the stack and can be
    ## inspected/browsed).
    path <- backup(target$name)
    withCallingHandlers(target_run(target, store, quiet),
                        error=function(e) {
                          restore(target$name, path)
                          stop(e)
                        })
    ## This only happens if the error is not raised above:
    target_set(target, store, NULL)
    invisible(target$name)
  } else if (target$type == "object") {
    res <- target_run(target, store, quiet)
    target_set(target, store, res)
    invisible(res)
  } else if (target$type == "cleanup") {
    target_level <- sapply(target$maker$targets, function(x) x$cleanup_level)
    will_remove <- names(target$maker$targets)[target_level == target$name]
    target$maker$remove_targets(will_remove)
    target_run(target, store, quiet)
  }
}

target_run <- function(target, store, quiet=NULL) {
  if (is.null(target$rule)) {
    return()
  } else if (target$type == "utility") {
    return(target$utility(target$maker))
  } else if (inherits(target, "target_knitr")) {
    object_names <- target$depends[target$depends_type == "object"]
    return(
      knitr_from_maker(target$knitr$input, target$name, store,
                       object_names,
                       quiet=with_default(quiet, target$quiet),
                       knitr_options=target$knitr$options,
                       chdir=target$knitr$chdir,
                       auto_figure_prefix=target$knitr$auto_figure_prefix))

  }

  if (inherits(target, "target_plot")) {
    open_device(target$plot$device, plot_args(target), store$env$env)
    on.exit(dev.off())
  }

  args <- dependencies_as_args(target, store)

  ## Setting quiet in a target always overrides any runtime
  ## option.
  ## TODO: quiet is not getting sanitised here.  Run via isTRUE?
  quiet <- with_default(quiet, target$quiet)
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
    do.call(target$rule, args, envir=store$env$env),
    message=function(e) if (quiet) invokeRestart("muffleMessage"))
}

target_activate <- function(target, maker) {
  if (target$type == "cleanup" || target$type == "maker") {
    target$maker <- maker
  } else if (inherits(target, "target_plot")) {
    ## NOTE: This is done during activate because we need access to
    ## the set of plot options.  That seems suboptimal.
    dev <- get_device(tools::file_ext(target$name))
    plot <- target$plot
    if (identical(plot, TRUE) || is.null(plot)) {
      plot <- list()
    } else if (is.character(plot) && length(plot) == 1) {
      if (plot %in% names(maker$plot_options)) {
        plot <- maker$plot_options[[plot]]
      } else {
        stop(sprintf("Unknown plot_options '%s' in target '%s'",
                     plot, target$name))
      }
    } else {
      assert_list(plot)
      assert_named(plot)
    }
    ## This will not work well for cases where `...` is in the
    ## device name (such as jpeg, bmp, etc)
    warn_unknown("plot", plot, names(formals(dev)))
    target$plot <- list(device=dev, args=plot)
  }
  if (length(target$depends) == 0L) {
    return(target)
  }

  ## This section matches the dependencies with their location in
  ## the database's set of targets. Missing file targets will be
  ## created and added to the database (which being passed by
  ## reference will propagate backwards).
  ##
  if (!is.null(target$chain_kids)) {
    maker$add_targets(target$chain_kids, activate=TRUE)
  }

  msg <- setdiff(target$depends, maker$target_names(all=TRUE))
  if (length(msg) > 0L) {
    err <- !target_is_file(msg)
    if (any(err)) {
      stop(sprintf(
        "Implicitly created targets must all be files (%s)",
        paste(msg[err], collapse=", ")))
    }
    implicit <- lapply(msg, target_new_file_implicit)
    names(implicit) <- msg
    maker$targets <- c(maker$targets, implicit)
  }

  target$depends_type <- dependency_types(maker$get_targets(target$depends))
  target_check_quoted(target)
  target
}

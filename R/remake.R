#' @importFrom utils packageName packageVersion browseURL download.file install.packages unzip zip
#' @importFrom grDevices col2rgb dev.cur dev.off rgb
remake <- function(remake_file="remake.yml", verbose=TRUE,
                   allow_cache=TRUE, load_sources=TRUE,
                   allow_missing_packages=FALSE) {
  if (is.null(remake_file)) {
    return(.R6_remake_interactive$new(verbose=verbose))
  }

  ## Dealing with caching here is tricky; we don't want to cache the
  ## allow_missing_packages bit.  So to do this nicely I'm just going
  ## to prevent adding it to the cache.  Not ideal, but it's an
  ## emergency option.
  if (allow_missing_packages || !allow_cache) {
    return(remake_new(remake_file, verbose,
                      load_sources, allow_missing_packages))
  }

  ret <- cache$fetch(remake_file)
  if (is.null(ret)) {
    ret <- remake_new(remake_file, verbose,
                      load_sources, allow_missing_packages)
    cache$add(ret)
  } else {
    ret$verbose <- remake_verbose(verbose)
    if (load_sources) {
      ret <- .remake_initialize_sources(ret)
    }
  }
  ret
}

remake_new <- function(remake_file="remake.yml", verbose=TRUE,
                       load_sources=TRUE, allow_missing_packages=FALSE,
                       config=NULL) {
  obj <- list(file=remake_file, path=".",
              verbose=remake_verbose(verbose),
              allow_missing_packages=allow_missing_packages,
              ##
              fmt=.remake_initialize_message_format(NULL),
              store=NULL, targets=NULL, config=NULL,
              default_target=NULL, hash=NULL)
  remake_print_message(obj, "LOAD", "")

  if (is.null(config) && !is.null(obj$file)) {
    obj$config <- read_remake_file(obj$file)
  } else {
    obj$config <- config
  }

  obj$hash <- obj$config$hash
  ## Do this when config is actualy there, and if it's not explicitly
  ## flagged as inactive.
  if (!is.null(obj$config) && !isFALSE(obj$config$active)) {
    obj <- .remake_initialize_targets(obj)
  }

  if (is.null(obj$config)) {
    packages <- sources <- character(0)
  } else {
    packages <- obj$config$packages
    sources  <- obj$config$sources
  }

  obj$store <- store$new(obj$path, packages, sources)

  if (load_sources) {
    obj <- .remake_initialize_sources(obj)
  }
  class(obj) <- "remake"
  obj
}

read_remake_file <- function(filename, seen=character(0)) {
  if (filename %in% seen) {
    stop("Recursive include detected: ",
         paste(c(seen, filename), collapse=" -> "))
  }

  ## TODO: Sort out the logic here:
  if (dirname(filename) != "." && dirname(filename) != getwd()) {
    stop("Logic around paths in out-of-directory remake files not decided")
  }

  dat <- yaml_read(filename)
  warn_unknown(filename, dat,
               c("packages", "sources", "include",
                 "plot_options", "knitr_options",
                 "file_extensions", "target_default", "targets"))

  dat$hash <- hash_files(filename, named=TRUE)

  if (length(seen) > 0L) { # an included file
    dat$target_default <- NULL
  }

  dat$packages <- with_default(dat$packages, character(0))
  dat$sources  <- with_default(dat$sources,  character(0))

  if (!is.null(dat$plot_options)) {
    assert_named_list(dat$plot_options)
    for (i in names(dat$plot_options)) {
      assert_named_list(dat$plot_options[[i]],
                        name=paste("plot_options: ", i))
    }
  }
  if (!is.null(dat$knitr_options)) {
    assert_named_list(dat$knitr_options)
    for (i in names(dat$knitr_options)) {
      assert_named_list(dat$knitr_options[[i]],
                        name=paste("knitr_options: ", i))
    }
  }

  if (is.null(dat$file_extensions) || length(dat$file_extensions) == 0L) {
    dat$file_extensions <- NULL
  } else {
    assert_character(dat$file_extensions)
    dat$file_extensions <- union(file_extensions(),
                                 sub("^\\.", "", dat$file_extensions))
  }

  if (!is.null(dat$include)) {
    assert_character(dat$include)
    ## TODO: Even after sorting out main file restriction, this one
    ## may need some work. Could rewrite file-based rules to adjust
    ## relative paths, or leave relative paths going against the main
    ## file.  Not sure what the right answer here is, so requiring new
    ## files to be in the current working directory.
    ##
    ## TODO: I think the correct answer is to assume everything
    ## relative to the main remakefile.  Possibly support a
    ## include-and-chdir approach too, where rules are rewritten to
    ## support relative paths, but that's going be hairy.
    if (any(dirname(dat$include) != ".")) {
      stop("All included remakefiles must be in the current directory")
    }
    for (f in dat$include) {
      dat_sub <- read_remake_file(f, c(seen, filename))
      dat$packages <- unique(c(dat$packages, dat_sub$packages))
      dat$sources  <- unique(c(dat$sources,  dat_sub$sources))
      ## No unique here because it destroys names.  There should be no
      ## repetition here though.
      dat$hash     <-        c(dat$hash,     dat_sub$hash)

      dups <- intersect(names(dat_sub$plot_options), names(dat$plot_options))
      if (length(dups) > 0L) {
        stop(sprintf("%s contains duplicate plot_options %s",
                     f, paste(dups, collapse=", ")))
      }
      dat$plot_options <- c(dat$plot_options, dat_sub$plot_options)

      dups <- intersect(names(dat_sub$knitr_options), names(dat$knitr_options))
      if (length(dups) > 0L) {
        stop(sprintf("%s contains duplicate knitr_options %s",
                     f, paste(dups, collapse=", ")))
      }
      dat$knitr_options <- c(dat$knitr_options, dat_sub$knitr_options)

      if ("all" %in% names(dat_sub$targets)) {
        warning(f, " contains target 'all', which I am removing")
        dat_sub$targets$all <- NULL
      }

      ## TODO: This will be a repeated pattern, similar to plot_options
      ## TODO: Should track which files have duplicates
      dups <- intersect(names(dat_sub$targets), names(dat$targets))
      if (length(dups) > 0L) {
        ## This will throw an error later on, but a warning here will
        ## make that easier to diagnose.
        warning(sprintf("%s contains duplicate targets %s",
                        f, paste(dups, collapse=", ")))
      }
      dat$targets  <- c(dat$targets, dat_sub$targets)
    }
  }

  if (length(seen) == 0L) { # main file only
    extra <- list(plot_options=dat$plot_options,
                  knitr_options=dat$knitr_options)
    dat$targets <- lnapply(dat$targets, make_target, extra=extra,
                           file_extensions=dat$file_extensions)
  }

  dat
}

## TODO: This needs a new name.
remake_default_target <- function(obj, target_name=NULL) {
  if (is.null(target_name)) {
    if (is.null(obj$default_target)) {
      stop(obj$file,
           " does not define 'target_default' or have target 'all'")
    }
    obj$default_target
  } else {
    target_name
  }
}

remake_print_message <- function(obj, status, target_name,
                                 cmd=NULL, style="square") {
  verbose <- obj$verbose
  if (!verbose$print_progress ||
      !verbose$print_noop && status %in% c("", "OK")) {
    return()
  } else if (!verbose$print_command) {
    cmd <- NULL
  }

  status <- brackets(paint(sprintf("%5s", status),
                           status_colour(status)), style)

  target_name <- abbreviate(target_name, obj$fmt$target_width)

  if (is.null(cmd)) {
    str <- sprintf("%s %s", status, target_name)
  } else {
    if (verbose$print_command_abbreviate) {
      w_extra <- max(0, nchar(target_name) - obj$fmt$target_width)
      cmd <- abbreviate(cmd, obj$fmt$max_cmd_width - w_extra)
    }
    str <- sprintf(obj$fmt$fmt, status, target_name, paint(cmd, "grey60"))
  }
  message(str)
}


remake_plan <- function(obj, target_name=NULL) {
  target_name <- remake_default_target(obj, target_name)
  graph <- remake_dependency_graph(obj)
  dependencies(target_name, graph)
}

remake_dependency_graph <- function(obj) {
  g <- lapply(obj$targets, function(t) t$depends_name)
  topological_sort(g)
}

remake_update <- function(obj, target_name, check=NULL,
                          return_target=TRUE) {
  target <- obj$targets[[target_name]]
  current <- remake_is_current(obj, target_name)

  if (!isTRUE(target$implicit)) {
    status <- if (current) "OK" else target$status_string
    cmd <- if (current) NULL else target_run_fake(target)
    style <- "square"
    remake_print_message(obj, status, target_name, cmd, style)
  }

  ret <- NULL
  if (target$type == "fake") {
    ## Just weed these out for now
  } else if (!current) {
    ## We'll load packages here because we might actually build
    ## something -- this can take a while on some projects (e.g.,
    ## richfitz/modeladequacy).
    .remake_initialize_packages(obj)
    ## See #12 - targets can specify conditional packages, and
    ## we load them, but also unload them afterwards (including
    ## dependencies).  This does not leave packages loaded for
    ## dependent taragets though.
    extra <- load_extra_packages(target$packages, obj$file)
    if (target$type == "cleanup") {
      ## Do this here because it uses the remake object (via
      ## remake_remove_target), which is not available in
      ## target_build.
      for (t in target$targets_to_remove) {
        remake_remove_target(obj, t)
      }
      target_run(target, obj$store, obj$verbose$quiet_target)
      ret <- NULL
    } else {
      ret <- target_build(target, obj$store, obj$verbose$quiet_target)
    }
    unload_extra_packages(extra)
  } else if (return_target) {
    ret <- target_get(target, obj$store)
  }

  # Return only if asked
  if (return_target) {
    invisible(ret)
  } else {
    invisible(NULL)
  }
}

## TODO: This is currently used by the clean targets.  The name
## probably wants changing though, because it's confusing with "add"
## that *creates* a target.  What this does is remove the target
## *product*.
remake_remove_target <- function(obj, target_name) {
  assert_has_targets(target_name, obj)
  target <- obj$targets[[target_name]]
  store <- obj$store

  if (target$type == "file") {
    did_remove_obj <- store$files$del(target$name)
    did_remove_db  <- store$db$del(target$name)
    did_remove <- did_remove_obj || did_remove_db
  } else if (target$type == "object") {
    did_remove_obj <- store$objects$del(target$name)
    did_remove_db  <- store$db$del(target$name)
    did_remove <- did_remove_obj || did_remove_db
  } else {
    stop("Not something that can be deleted")
  }

  if (did_remove) {
    status <- "DEL"
    fn <- if (target$type == "object") "rm" else "file.remove"
    cmd <- sprintf('%s("%s")', fn, target_name)
  } else {
    status <- ""
    cmd <- NULL
  }
  remake_print_message(obj, status, target_name, cmd, "round")
}

remake_make <- function(obj, target_names=NULL, ...) {
  target_names <- remake_default_target(obj, target_names)
  for (t in target_names) {
    remake_print_message(obj, "MAKE", t, style="angle")
  }
  remake_make1(obj, target_names, ...)
}

remake_make1 <- function(obj, target_name, check=NULL) {
  last_target_name <- utils::tail(target_name, 1L)
  plan <- remake_plan(obj, target_name)
  ret <- lapply(plan, function(i) {
    is_last <- i == last_target_name
    last <- remake_update(obj, i, check=check, return_target=is_last)
  })
  invisible(ret[plan == last_target_name][[1L]])
}

remake_list_targets <- function(obj, type=NULL,
                                include_implicit_files=FALSE,
                                include_cleanup_targets=FALSE) {
  filter_targets(obj$targets,
                 type,
                 include_implicit_files,
                 include_cleanup_targets)
}

remake_list_dependencies <- function(obj, target_names, type=NULL,
                                     include_implicit_files=FALSE,
                                     include_cleanup_targets=FALSE) {
  ## TODO: Perhaps this should  be done by dependencies?
  if (!all(target_names %in% names(obj$targets))) {
    stop("Unknown target: ",
         paste(setdiff(target_names, names(obj$targets)), collapse=", "))
  }
  graph <- remake_dependency_graph(obj)
  target_names <- dependencies(target_names, graph)
  filter_targets(obj$targets[target_names],
                 type,
                 include_implicit_files,
                 include_cleanup_targets)
}

remake_is_current <- function(obj, target_names, check=NULL) {
  assert_has_targets(target_names, obj)
  vlapply(obj$targets[target_names], function(x)
    target_is_current(x, obj$store, check), USE.NAMES=FALSE)
}

remake_who_refers_to <- function(obj, target_names) {
  deps <- lapply(obj$targets, "[[", "depends_name")
  mat <- vapply(target_names, function(e)
    vlapply(deps, function(x) e %in% x), logical(length(deps)))
  unname(apply(mat, 2, function(x) paste(names(deps)[x], collapse=", ")))
}

remake_dump_environment <- function(obj, envir) {
  assert_environment(envir)
  remake_print_message(obj, "DUMP", "")
  ## TODO: This will change once ported over to callr's source
  ## functions.  Note that there is unwanted duplication from
  ## .remake_initialize_packages.
  packages <- obj$store$packages
  if (obj$allow_missing_packages) {
    packages <- intersect(packages, .packages(TRUE))
  }
  load_packages(packages)
  for (f in obj$store$env$source_files) {
    sys.source(f, envir, chdir=TRUE)
  }
  obj$store$objects$export(envir)
}

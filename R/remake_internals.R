## Internal support funtions used in remake creation.
.remake_initialize_packages <- function(obj) {
  ## NOTE: This one here is working entirely by reference, but still
  ## must return the object
  if (!obj$store$packages_loaded) {
    remake_print_message(obj, "READ", "", "# loading packages")
    packages <- obj$store$packages
    if (obj$allow_missing_packages) {
      packages <- intersect(packages, .packages(TRUE))
    }
    load_packages(packages, obj$file)
    obj$store$packages_loaded <- TRUE

    ## TODO: This might want to go earlier when the remake file is
    ## loaded I think.
    missing_target_packages <- missing_packages(target_packages(obj))
    if (length(missing_target_packages) > 0L &&
        getOption("remake.warn.missing.target.packages", TRUE)) {
      str <- missing_package_instructions(missing_target_packages,
                                          obj$file, target_specific=TRUE)
      warning(str, call.=FALSE, immediate.=TRUE)
    }
  }
  obj
}

.remake_initialize_sources <- function(obj) {
  ## NOTE: This one here is working entirely by reference, but still
  ## must return the object
  if (!is.null(obj$store$env) && !obj$store$env$is_current()) {
    ## TODO: target specific packages?  Perhaps with
    ## remake.warn.missing.target.packages?
    if (!obj$allow_missing_packages) {
      warn_missing_packages(obj$store$packages)
    }
    remake_print_message(obj, "READ", "", "# loading sources")
    obj$store$env$reload()
    global_active_bindings$reload_bindings("source", obj)
  }
  obj
}

.remake_add_targets <- function(obj, targets, force=FALSE) {
  if (!all(vlapply(targets, inherits, "target_base"))) {
    stop("All elements must be targets")
  }
  target_names <- vcapply(targets, "[[", "name")
  if (any(duplicated(target_names))) {
    stop("All target names must be unique")
  }

  obj_targets <- obj$targets
  target_names_existing <- names(obj_targets)
  if (any(target_names %in% target_names_existing)) {
    if (force) {
      to_drop <- target_names_existing %in% target_names
      if (any(to_drop)) {
        obj_targets <- obj_targets[!to_drop]
      }
    } else {
      stop("Targets already present: ",
           paste(intersect(target_names, target_names_existing),
                 collapse=", "))
    }
  }
  names(targets) <- target_names
  c(obj_targets, targets)
}

.remake_initialize_message_format <- function(obj) {
  width <- getOption("width")
  keep <- !vlapply(obj$targets, function(x) isTRUE(x$implicit))
  target_width <- max(0, nchar(names(obj$targets)[keep]))
  list(
    fmt=sprintf("%%s %%-%ds |  %%s", target_width),
    target_width=target_width,
    max_cmd_width=width - (nchar("[ BUILD ] ") + 1 + target_width + 4))
}

## Used only in `refresh`
.remake_initialize_targets <- function(obj) {
  obj$targets <- NULL
  obj$targets <- .remake_add_targets(obj, obj$config$targets)

  ## TODO: Should cleanup targets go last?
  cleanup_targets <- lapply(cleanup_target_names(),
                            make_target_cleanup, obj)
  obj$targets <- .remake_add_targets(obj, cleanup_targets, force=TRUE)
  obj$targets <- .remake_add_targets_implied(obj)

  ## Widths might have changed:
  obj$fmt <- .remake_initialize_message_format(obj)
  
  ## Check that no rules and target names clash: this will cause a
  ## problem for export of variables.
  ## TODO: Filter by realness?
  ## TODO: Also possible that function names and targets will clash
  ## when using global variables, but that will get taken care of
  ## during loading.
  rules <- unlist(lapply(obj$targets, function(x) x$rule))
  dups <- intersect(rules, names(obj$targets))
  if (length(dups) > 0L) {
    warning("Rule name clashes with target name: ",
            paste(dups, collapse=", "))
  }

  .remake_check_target_literal_clash(obj)

  ## Check the default target:
  default <- obj$config$target_default
  if (is.null(default)) {
    if ("all" %in% names(obj$targets)) {
      obj$default_target <- "all"
    }
  } else {
    assert_scalar_character(default, "target_default")
    if (!(default %in% names(obj$targets))) {
      stop(sprintf("Default target %s not found in remakefile",
                   default))
    }
    obj$default_target <- default
  }

  global_active_bindings$reload_bindings("target", obj)
  obj
}

## This is only used in initialize targets, but it's sufficiently ugly
## that I've pulled it out on its own.  The job here is to identify
## and add all *implied* targets; these are steps in implicit file
## targets.
.remake_add_targets_implied <- function(obj) {
  obj_targets <- obj$targets

  ## Identify and verify all "implicit" file targets
  deps <- lapply(obj_targets, "[[", "depends_name")
  deps_uniq <- unique(unlist(unname(deps)))
  deps_msg <- setdiff(deps_uniq, names(obj_targets))
  if (length(deps_msg) > 0L) {
    err <- !target_is_file(deps_msg, obj$file_extensions)
    if (any(err)) {
      err_names <- deps_msg[err]
      err_used <- remake_who_refers_to(obj, err_names)
      msg <- sprintf(" - %s: (in %s)", err_names, err_used)
      if (getOption("remake.dym", TRUE)) {
        pos <- unique(c(names(obj$targets),
                        setdiff(deps_uniq, deps_msg)))
        ## Consider checking files too?
        ##   pos <- unique(c(pos, list.files(recursive=TRUE)))
        suggestion <- did_you_mean(err_names, pos, " -- did you mean: ")
        msg <- paste0(msg, suggestion)
      }
      stop(paste(c("Implicitly created targets must all be files:", msg),
                   collapse="\n"))
    }
    deps_msg_missing <- !file.exists(deps_msg)
    if (any(deps_msg_missing)) {
      msg_names <- deps_msg[deps_msg_missing]
      msg_used <- remake_who_refers_to(obj, msg_names)
      msg <- sprintf(" - %s: (in %s)", msg_names, msg_used)
      if (getOption("remake.dym", TRUE)) {
        pos <- unique(c(names(obj$targets),
                        setdiff(deps_uniq, deps_msg)))
        ## Consider checking files too?
        ##   pos <- unique(c(pos, list.files(recursive=TRUE)))
        suggestion <- did_you_mean(msg_names, pos, " -- did you mean: ")
        msg <- paste0(msg, suggestion)
      }
      warning(paste(c("Creating implicit target for nonexistant files",
                      msg), collapse="\n"),
              immediate.=TRUE)
    }
    extra <- lapply(deps_msg, target_new_file_implicit, FALSE)
    names(extra) <- deps_msg
    obj_targets <- c(obj_targets, extra)
  }

  ## Associate all type information for targets (this is the slow part)
  types <- vcapply(obj_targets, "[[", "type")
  check1 <- function(t) {
    if (length(t$depends_name) > 0L) {
      t$depends_type <- types[t$depends_name]
      target_check_quoted(t)
    }
    t
  }
  lapply(obj_targets, check1)
}

.remake_check_target_literal_clash <- function(obj) {
  ## Check that literal things don't clash with targets.
  target_literal_clash <- function(t) {
    ret <- character(0)
    pos <- !t$arg_is_target
    if (any(pos)) {
      literal <- as.list(t$command[-1][pos])
      i <- vlapply(literal, is.symbol)
      if (any(i)) {
        ret <- intersect(vcapply(literal[i], as.character),
                         names(obj$targets))
      }
    }
    ret
  }
  clash <- lapply(obj$targets, target_literal_clash)
  i <- viapply(clash, length) > 0L
  if (any(i)) {
    err <- sprintf(" - %s: %s",
                   names(obj$targets)[i],
                   vcapply(clash[i], paste, collapse=", "))
    stop("target/literal clash:\n", paste(err, collapse="\n"))
  }
}

cleanup_levels <- function() {
  c("tidy", "clean", "purge", "never")
}

cleanup_target_names <- function() {
  c("tidy", "clean", "purge")
}

## TODO: KNIT was hotpink, but that's broken on OSX
## TODO: I get quite different colours on different platforms at the
## moment.
status_colour <- function(str) {
  switch(str,
         BUILD="steelblue4",
         OK="green3",
         CLEAN="orange",
         DEL="red1",
         UTIL="darkorchid3",
         LOAD="yellow1",
         READ="yellow1",
         PLOT="dodgerblue2",
         KNIT="dodgerblue4",
         DLOAD="dodgerblue2",
         MAKE="deepskyblue",
         ENV="deepskyblue",
         ZIP="darkorchid3",
         UNZIP="slateblue2",
         DUMP="darkorchid3",
         TIME="pink3",
         "-----"="grey60",
         NULL)
}

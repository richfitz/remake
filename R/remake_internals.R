## Internal support funtions used in remake creation.
.remake_initialize_sources <- function(obj) {
  ## NOTE: This one here is working entirely by reference.
  if (!is.null(obj$store$env) && !obj$store$env$is_current()) {
    remake_print_message(obj, "READ", "", "# loading sources")
    tryCatch(obj$store$env$reload(TRUE),
             missing_packages=function(e) missing_packages_recover(e, obj))
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
  w0 <- 10 # nchar("[ BUILD ] ")
  keep <- !vlapply(obj$targets, function(x) isTRUE(x$implicit))
  target_width <- max(0, nchar(names(obj$targets)[keep]))
  list(
    no_cmd="%s %s",
    with_cmd=sprintf("%%s %%-%ds |  %%s", target_width),
    target_width=target_width,
    max_cmd_width=width - (w0 + 1 + target_width + 4))
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
  ## TODO: Special effort needed for chained rules.
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
## and add all *implied* targets; these are steps in chains and
## implicit file targets.
.remake_add_targets_implied <- function(obj) {
  obj_targets <- obj$targets
  ## Add all targets that exist only as part of a chain.
  chain_kids <- unlist(lapply(obj_targets, "[[", "chain_kids"),
                       FALSE)
  if (length(chain_kids) > 0L) {
    obj_targets <- .remake_add_targets(obj, chain_kids)
  }

  ## Identify and verify all "implicit" file targets
  deps <- lapply(obj_targets, "[[", "depends_name")
  deps_uniq <- unique(unlist(unname(deps)))
  deps_msg <- setdiff(deps_uniq, names(obj_targets))
  if (length(deps_msg) > 0L) {
    err <- !target_is_file(deps_msg)
    if (any(err)) {
      stop(sprintf(
        "Implicitly created targets must all be files (%s)",
        paste(deps_msg[err], collapse=", ")))
    }
    deps_msg_missing <- !file.exists(deps_msg)
    if (any(deps_msg_missing)) {
      warning("Creating implicit target for nonexistant files:\n",
              paste0("\t", deps_msg[deps_msg_missing], collapse="\n"))
    }
    extra <- lapply(deps_msg, target_new_file_implicit, FALSE)
    names(extra) <- deps_msg
    obj_targets <- c(obj_targets, extra)
  }

  ## Associate all type information for targets (this is the slow part)
  types <- dependency_types(obj_targets)
  check1 <- function(t) {
    if (length(t$depends_name) > 0L) {
      t$depends_type <- types[t$depends_name]
      target_check_quoted(t)
    }
    t
  }
  lapply(obj_targets, check1)
}

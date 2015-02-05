##' @importFrom R6 R6Class
.R6_maker <- R6Class(
  "maker",
  public=list(
    store=NULL,
    targets=NULL,

    initialize=function(maker_file="maker.yml", verbose=TRUE, envir=NULL) {
      #
      private$file <- maker_file
      private$path <- "."
      private$verbose <- maker_verbose(verbose)
      if (!is.null(envir)) {
        private$active_bindings <- maker_active_bindings_manager(envir)
      }
      private$initialize_message_format()
      private$refresh()
    },

    make=function(target_names=NULL, ...) {
      private$refresh()
      if (is.null(target_names)) {
        target_names <- private$target_default()
      }
      for (t in target_names) {
        private$print_message("MAKE", t, style="angle")
        last <- private$make1(t, ...)
      }
      invisible(last)
    },

    target_names=function(all=FALSE) {
      if (!all) {
        ok <- vlapply(self$targets, function(x) is.null(x$chain_parent))
        names(self$targets[ok])
      } else {
        names(self$targets)
      }
    }
  ),

  private=list(
    file=NULL,
    path=NULL,
    verbose=NULL,
    default_target=NULL,

    hash=NULL,
    active_bindings=NULL,

    fmt=NULL,

    refresh=function() {
      config <- private$read_config()
      if (!is.null(config)) {
        ## NOTE: probably best not to print this the first time?  That
        ## would be when private$hash is empty.
        if (!is.null(private$hash)) {
          private$print_message("READ", "", "# reloading makerfile")
        }
        private$reload_config(config)
      } else {
        private$initialize_sources()
      }
    },

    read_config=function() {
      reload <- is.null(private$hash) ||
        !identical(hash_files(names(private$hash)), private$hash)
      if (reload) {
        read_maker_file(private$file)
      } else {
        NULL
      }
    },

    reload_config=function(config) {
      private$hash <- config$hash
      private$initialize_targets(config)
      private$initialize_store(config)
      private$initialize_message_format()
      private$initialize_sources()
    },

    initialize_targets=function(config) {
      self$targets <- NULL
      private$add_targets(config$targets)
      private$initialize_cleanup_targets()
      private$initialize_targets_activate()
      private$check_rule_target_clash()
      private$initialize_default_target(config$target_default)
      if (!is.null(private$active_bindings)) {
        maker_reload_active_bindings(self, "target", private$active_bindings)
      }
    },

    initialize_store=function(config) {
      self$store <- store$new(private$path)
      self$store$env <- managed_environment$new(config$packages, config$sources)
    },

    initialize_sources=function() {
      if (!self$store$env$is_current()) {
        private$print_message("READ", "", "# loading sources")
        tryCatch(self$store$env$reload(TRUE),
                 missing_packages=function(e) missing_packages_recover(e, self))
        if (!is.null(private$active_bindings)) {
          maker_reload_active_bindings(self, "source", private$active_bindings)
        }
      }
    },

    initialize_cleanup_targets=function() {
      targets <- lapply(cleanup_target_names(), make_target_cleanup, self)
      private$add_targets(targets, force=TRUE)
    },

    ## NOTE: The logic here seems remarkably clumsy.
    initialize_default_target=function(default) {
      if (is.null(default)) {
        if ("all" %in% self$target_names()) {
          private$default_target <- "all"
        }
      } else {
        assert_scalar_character(default, "target_default")
        if (!(default %in% self$target_names())) {
          stop(sprintf("Default target %s not found in makerfile",
                       default))
        }
        private$default_target <- default
      }
    },

    initialize_targets_activate=function() {
      ## Add all targets that exist only as part of a chain.
      chain_kids <- unlist(lapply(self$targets, "[[", "chain_kids"),
                           FALSE)
      if (length(chain_kids) > 0L) {
        private$add_targets(chain_kids)
      }

      ## Identify and verify all "implicit" file targets
      deps <- lapply(self$targets, "[[", "depends_name")
      deps_uniq <- unique(unlist(unname(deps)))
      deps_msg <- setdiff(deps_uniq, names(self$targets))
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
        self$targets <- c(self$targets, extra)
      }

      ## Associate all type information for targets (this is the slow part)
      types <- dependency_types(self$targets)
      check1 <- function(t) {
        if (length(t$depends_name) > 0L) {
          t$depends_type <- types[t$depends_name]
          target_check_quoted(t)
        }
        t
      }
      self$targets <- lapply(self$targets, check1)
    },

    initialize_message_format=function() {
      width <- getOption("width")
      w0 <- 10 # nchar("[ BUILD ] ")
      keep <- !vlapply(self$targets, function(x) isTRUE(x$implicit))
      target_width <- max(0, nchar(names(self$targets)[keep]))
      private$fmt <- list(
        no_cmd="%s %s",
        with_cmd=sprintf("%%s %%-%ds |  %%s", target_width),
        target_width=target_width,
        max_cmd_width=width - (w0 + 1 + target_width + 4))
    },

    check_rule_target_clash=function() {
      ## TODO: Special effort needed for chained rules.
      ## TODO: Filter by realness?
      rules <- unlist(lapply(self$targets, function(x) x$rule))
      dups <- intersect(rules, self$target_names())
      if (length(dups) > 0L) {
        warning("Rule name clashes with target name: ",
                paste(dups, collapse=", "))
      }
    },

    target_default=function() {
      if (is.null(private$default_target)) {
        stop(private$file,
             " does not define 'target_default' or have target 'all'")
      }
      private$default_target
    },

    add_targets=function(x, force=FALSE) {
      if (!all(vlapply(x, inherits, "target_base"))) {
        stop("All elements must be targets")
      }
      target_names <- vcapply(x, "[[", "name")
      if (any(duplicated(target_names))) {
        stop("All target names must be unique")
      }
      if (any(target_names %in% self$target_names(all=TRUE))) {
        if (force) {
          to_drop <- self$target_names() %in% target_names
          if (any(to_drop)) {
            self$targets <- self$targets[!to_drop]
          }
        } else {
          stop("Targets already present: ",
               paste(intersect(target_names, self$target_names()),
                     collapse=", "))
        }
      }
      names(x) <- target_names
      self$targets <- c(self$targets, x)
    },

    print_message=function(status, target_name, cmd=NULL, style="square") {
      verbose <- private$verbose
      if (!verbose$print_progress ||
          !verbose$print_noop && status %in% c("", "OK")) {
        return()
      } else if (!verbose$print_command) {
        cmd <- NULL
      }

      status <- brackets(paint(status, status_colour(status)), style)

      if (!is.null(cmd)) {
        if (verbose$print_command_abbreviate) {
          w_extra <- max(0, nchar(target_name) - private$fmt$target_width)
          cmd <- abbreviate(cmd, private$fmt$max_cmd_width - w_extra)
        }
      }
      if (is.null(cmd)) {
        str <- sprintf(private$fmt$no_cmd, status, target_name)
      } else {
        str <- sprintf(private$fmt$with_cmd, status, target_name,
                       paint(cmd, "grey60"))
      }
      message(str)
    },

    get_target=function(target_name) {
      if (!(target_name %in% names(self$targets))) {
        stop("No such target ", target_name)
      }
      self$targets[[target_name]]
    },

    plan=function(target_name=NULL, dependencies_only=FALSE) {
      if (is.null(target_name)) {
        target_name <- private$target_default()
      }
      graph <- private$dependency_graph()
      dependencies(target_name, graph, dependencies_only)
    },

    dependency_graph=function() {
      g <- lapply(self$targets, function(t) t$depends_name)
      topological_sort(g)
    },

    make1=function(target_name, dry_run=FALSE, force=FALSE,
      force_all=FALSE, quiet_target=private$verbose$quiet_target, check=NULL,
      dependencies_only=FALSE) {
      #
      private$refresh()
      plan <- private$plan(target_name, dependencies_only)
      for (i in plan) {
        is_last <- i == target_name
        last <- private$update(i, dry_run,
                               force_all || (force && is_last),
                               quiet_target=quiet_target, check=check,
                               return_target=is_last)
      }
      invisible(last)
    },

    update=function(target_name, dry_run=FALSE, force=FALSE,
      quiet_target=private$verbose$quiet_target, check=NULL,
      return_target=TRUE) {
      #
      target <- private$get_target(target_name) # self$targets[[target_name]]
      current <- !force && target_is_current(target, self$store, check)

      if (!isTRUE(target$implicit)) {
        status <- if (current) "OK" else target$status_string
        cmd <- if (current) NULL else target_run_fake(target)
        style <- if (is.null(target$chain_parent)) "square" else "curly"
        private$print_message(status, target_name, cmd, style)
      }

      if (!dry_run) {
        if (!current) {
          ## See #12 - targets can specify conditional packages, and
          ## we load them, but also unload them afterwards (including
          ## dependencies).  This does not leave packages loaded for
          ## dependent taragets though.
          extra <- load_extra_packages(target$packages)
          if (target$type == "cleanup") {
            ## Do this here because it uses maker (via
            ## private$remove_target).
            for (t in target$targets_to_remove) {
              private$remove_target(t, chain=TRUE)
            }
            target_run(target, self$store, quiet_target)
            ret <- NULL
          } else {
            ret <- target_build(target, self$store, quiet_target)
          }
          unload_extra_packages(extra)
          invisible(ret)
        } else if (return_target) {
          invisible(target_get(target, self$store))
        }
      }
    },

    ## TODO: This is currently used by the clean targets.  The name
    ## probably wants changing though, because it's confusing with
    ## $add that *creates* a target.  What this does is remove the
    ## target *product*.
    remove_target=function(target_name, chain=TRUE) {
      target <- private$get_target(target_name)
      if (chain && !is.null(target$chain_kids)) {
        chain_names <- dependency_names(target$chain_kids)
        for (t in chain_names) {
          private$remove_target(t, chain=FALSE)
        }
      }

      store <- self$store

      if (target$type == "file") {
        did_remove_obj <- store$files$del(target$name, TRUE)
        did_remove_db  <- store$db$del(target$name, TRUE)
        did_remove <- did_remove_obj || did_remove_db
      } else if (target$type == "object") {
        did_remove_obj <- store$objects$del(target$name, TRUE)
        did_remove_db  <- store$db$del(target$name, TRUE)
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
      private$print_message(status, target_name, cmd, "round")
    }
  ))

.R6_maker_interactive <- R6Class(
  "maker_interactive",
  inherit=.R6_maker,
  public=list(
    active=FALSE,

    initialize=function(verbose=TRUE, envir=NULL) {
      private$interactive <- maker_interactive_config()
      super$initialize(NULL, verbose, envir)
    },

    make=function(target_names=NULL, ...) {
      self$active <- TRUE
      super$make(target_names, ...)
    }
  ),

  active=list(
    add=function(value) {
      if (missing(value)) {
        message("Pass in libary/source/target calls here")
      } else  if (inherits(value, "target_base")) {
        maker_add_target(self, value)
      } else if (is.character(value)) {
        maker_add_sources(self, value)
      } else {
        stop("Can't add objects of class: ",
             paste(class(value), collapse=" / "))
      }
    }
  ),

  private=list(
    interactive=NULL,

    read_config=function() {
      config <- NULL
      reload <- !identical(hash_object(private$interactive), private$hash)
      if (reload) {
        config <- private$interactive
        config$hash <- hash_object(private$interactive)
      }
      config
    },

    initialize_targets=function(config) {
      if (self$active) {
        super$initialize_targets(config)
      }
    }
  ))

##' Creates a maker instance to interact with.
##' @title Create a maker object
##' @param maker_file Name of the makerfile (by default
##' \code{maker.yml})
##' @param verbose Controls whether maker is verbose or not.  By
##' default it is (\code{TRUE}), which prints out the name of each
##' target as it is built/checked.  This argument is passed to
##' \code{\link{maker_verbose}}; valid options are \code{TRUE},
##' \code{FALSE} and also the result of calling \code{maker_verbose}.
##' @param envir An environment into which to create \emph{links} to
##' maker-controlled objects (targets and sources).  \code{.GlobalEnv}
##' is a reasonable choice.  This will change in a future version.
##' @examples
##' \dontrun{
##' # create a quiet maker instance
##' m <- maker(verbose=FALSE)
##' # create a fairly quiet instance that does not print information
##' # for targets that do nothing (are up-to-date).
##' m <- maker(verbose=maker_verbose(noop=FALSE))
##' # Build the default target:
##' m$make()
##' }
##' @export
maker <- function(maker_file="maker.yml", verbose=TRUE, envir=NULL) {
  if (is.null(maker_file)) {
    .R6_maker_interactive$new(verbose=verbose, envir=envir)
  } else {
    .R6_maker$new(maker_file, verbose=verbose, envir=envir)
  }
}

## TODO: There is far too much going on in here: split this into
## logical chunks.
read_maker_file <- function(filename, seen=character(0)) {
  if (filename %in% seen) {
    stop("Recursive include detected: ",
         paste(c(seen, filename), collapse=" -> "))
  }

  ## TODO: Sort out the logic here:
  if (dirname(filename) != "." && dirname(filename) != getwd()) {
    stop("Logic around paths in out-of-directory maker files not decided")
  }

  dat <- yaml_read(filename)
  warn_unknown(filename, dat,
               c("packages", "sources", "include",
                 "plot_options", "knitr_options",
                 "target_default", "targets"))

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

  if (!is.null(dat$include)) {
    assert_character(dat$include)
    ## TODO: Even after sorting out main file restriction, this one
    ## may need some work. Could rewrite file-based rules to adjust
    ## relative paths, or leave relative paths going against the main
    ## file.  Not sure what the right answer here is, so requiring new
    ## files to be in the current working directory.
    ##
    ## TODO: I think the correct answer is to assume everything
    ## relative to the main makerfile.  Possibly support a
    ## include-and-chdir approach too, where rules are rewritten to
    ## support relative paths, but that's going be hairy.
    if (any(dirname(dat$include) != ".")) {
      stop("All included makerfiles must be in the current directory")
    }
    for (f in dat$include) {
      dat_sub <- read_maker_file(f, c(seen, filename))
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
    dat$targets <- lnapply(dat$targets, make_target, extra=extra)
  }

  dat
}

cleanup_levels <- function() {
  c("tidy", "clean", "purge", "never")
}

cleanup_target_names <- function() {
  c("tidy", "clean", "purge")
}

## Not sure I have a full list of these yet:
status_colour <- function(str) {
  switch(str,
         BUILD="steelblue4",
         OK="green3",
         CLEAN="orange",
         DEL="red1",
         UTIL="darkorchid3",
         READ="yellow1",
         PLOT="dodgerblue2",
         KNIT="hotpink",
         MAKE="deepskyblue",
         ENV="deepskyblue",
         "-----"="grey60",
         NULL)
}

##' @importFrom crayon make_style
paint <- function(str, col) {
  if (is.null(col)) {
    str
  } else {
    make_style(col)(str)
  }
}

##' Helper function to set options for verbosity.
##'
##' The first four options have a natural nesting: setting
##' \code{progress=FALSE} prevents printing any progress information,
##' so the value of \code{noop}, \code{command} and
##' \code{command_abbreviate} does not matter.  Similarly, setting
##' \code{command=FALSE} means that \code{command_abbreviate} does not
##' matter.
##' @title Control maker verbosity
##' @param verbose Print progress at each step that maker does
##' something.
##' @param noop Print progress for steps that are non-operations, such
##' as targets that need nothing done to them.  Setting this to
##' \code{FALSE} is useful for very large projects.
##' @param command Print the command along with the progress
##' information?  This is only printed when maker actually runs
##' something.
##' @param command_abbreviate Abbreviate the command information so
##' that it fits on one line.  If \code{FALSE} then the command will
##' be allowed to run on for as many lines as required.
##' @param target Print information that the target produces (via
##' \code{message}, \code{cat} or \code{print}).  If \code{FALSE} then
##' these messages will be suppressed.
##' @export
maker_verbose <- function(verbose=getOption("maker.verbose", TRUE),
                          noop=getOption("maker.verbose.noop", TRUE),
                          command=getOption("maker.verbose.command", TRUE),
                          command_abbreviate=TRUE,
                          target=NULL) {
  if (inherits(verbose, "maker_verbose")) {
    verbose
  } else {
    assert_scalar_logical(verbose)
    assert_scalar_logical(noop)
    assert_scalar_logical(command)
    assert_scalar_logical(command_abbreviate)
    if (!is.null(target)) {
      assert_scalar_logical(target)
      target <- !target
    }
    structure(list(print_progress=verbose,
                   print_noop=noop,
                   print_command=command,
                   print_command_abbreviate=command_abbreviate,
                   quiet_target=target),
              class="maker_verbose")
  }
}

## Helper function to access the private fields of a maker object.
## This is going to let us present a simple external interface to
## maker by allowing free functions (not just members) to access the
## maker internals.  The public/private gap here represents interface
## vs implementation detail.
##
## This helper will also get used extensively in tests.
maker_private <- function(m) {
  environment(m$initialize)$private
}

##' Check if a target is current or not.
##' @title Check if a target is current or not
##' @param target_name Name of the target.  An error is thrown if the
##' target does not exist.
##' @param m A maker object.  If omitted, then one will be built using
##. the defaults in \code{\link{maker}}
##' @param check What to check.  It can be "exists", "depends", "code"
##' or "all.  By default, this comes from the target default.
##' @export
##' @author Rich FitzJohn
is_current <- function(target_name, m=NULL, check=NULL) {
  if (is.null(m)) {
    m <- maker()
  }
  assert_has_target(target_name, m)
  target_is_current(m$targets[[target_name]], m$store, check)
}

assert_has_target <- function(target_name, m) {
  if (!(target_name %in% names(m$targets))) {
    stop("No such target ", target_name)
  }
}

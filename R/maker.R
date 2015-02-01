##' @importFrom R6 R6Class
.R6_maker <- R6Class(
  "maker",
  public=list(
    file=NULL,
    hash=NULL,
    path=NULL,
    store=NULL,
    targets=NULL,
    interactive=NULL,
    verbose=NULL,
    default=NULL,
    active_bindings=list(target=character(0), source=character(0)),
    envir=NULL,

    initialize=function(maker_file="maker.yml", verbose=TRUE, envir=NULL) {
      #
      self$file <- maker_file
      self$path <- "."
      self$verbose <- maker_verbose(verbose)
      if (!is.null(envir)) {
        assert_environment(envir)
        self$envir <- envir
      }
      if (self$is_interactive()) {
        self$interactive <- maker_interactive()
      } else {
        self$reload()
      }
    },

    is_interactive=function() {
      is.null(self$file)
    },

    reload=function() {
      self$store <- store$new(self$path)
      if (self$is_interactive()) {
        config <- self$interactive
        config$hash <- hash_object(self$interactive)
      } else {
        config <- read_maker_file(self$file)
      }
      self$hash <- config$hash
      self$targets <- NULL
      if (!(self$is_interactive() && !self$interactive$active)) {
        self$add_targets(config$targets)
        private$initialize_cleanup_targets()
        private$initialize_targets_activate()
        private$check_rule_target_clash()
        private$initialize_default_target(config$target_default)
      }
      private$initialize_message_format()
      self$store$env <- managed_environment$new(config$packages, config$sources)
      if (!is.null(self$envir)) {
        ## TODO: By default we don't run load_sources here: that's
        ## potentially a bit confusing because functions aren't made
        ## available.  I suspect that we should run load_sources here
        ## and load the functions: what's less clear is if that should
        ## only be done when given an environment or done always.
        ##
        ##     self$load_sources()
        ##     maker_reload_active_bindings(self, "source")
        ##
        ## Another option would be to put out just the rule names as
        ## special bindings.  That option I like the least though.
        ##
        ## A third option is to only dump out active bindings during
        ## load_sources itself.  That might be the most consistent.
        maker_reload_active_bindings(self, "target")
      }
    },

    make=function(target_names=NULL, ...) {
      ## TODO: This is here because of an issue with interactive
      ## getting access to the print function.  It's duplicated with
      ## make1.  That's probably going to represent a time-sink at
      ## some point; perhaps farm out to users ot make1?
      ##
      ## TODO: Need to check if interactive that we are activated.  Or
      ## perhaps we should activate at this point.  This is a bit of a
      ## trick with the active bindings because accessing one of those
      ## should probably not trigger a build.
      if (self$is_interactive()) {
        ## TODO: Check that this does about the right thing:
        self$interactive$active <- TRUE
      }
      self$load_sources()
      if (is.null(target_names)) {
        target_names <- self$target_default()
      }
      for (t in target_names) {
        self$print_message("MAKE", t, style="angle")
        last <- self$make1(t, ...)
      }
      invisible(last)
    },

    ## NOTE: This one is doing rather a lot more than the case below.
    ## The logic around here is subject to complete change, too.
    make_dependencies=function(target_name, ...) {
      t <- self$get_target(target_name)
      ## TODO: I don't see the big problem here:
      if (!(t$type) %in% c("object", "file")) {
        warning(sprintf("%s is not a real target"))
      }
      self$print_message("ENV", t$name, style="angle")
      self$make1(target_name, ..., dependencies_only=TRUE)
      deps_name <- t$depends_name[t$depends_type == "object"]

      invisible(maker_environment(self, deps_name, t))
    },

    ## TODO: Some support for getting *everything* out here.
    ##   - that's hard because we'd only want to get the up-to-date
    ##     things, and some of those might be large.
    ## TODO: Some support for doing a delayedAssign
    environment=function(target_names) {
      self$print_message("ENV", "", style="angle")
      self$load_sources()
      maker_environment(self, target_names, NULL)
    },

    make1=function(target_name, dry_run=FALSE, force=FALSE,
      force_all=FALSE, quiet_target=self$verbose$quiet_target, check=NULL,
      dependencies_only=FALSE) {
      #
      self$load_sources()
      plan <- self$plan(target_name, dependencies_only)
      for (i in plan) {
        is_last <- i == target_name
        last <- self$update(i, dry_run,
                            force_all || (force && is_last),
                            quiet_target=quiet_target, check=check,
                            return_target=is_last)
      }
      invisible(last)
    },

    script=function(target_name=NULL) {
      if (is.null(target_name)) {
        target_name <- self$target_default()
      }
      pkgs <- lapply(self$store$env$packages,
                     function(x) sprintf('library("%s")', x))
      ## TODO: This does not work for *directories*.  Emit the body of
      ## source_dir with appropriate things set.
      srcs <- lapply(self$store$env$sources,
                     function(x) sprintf('source("%s")', x))
      ## Probably best to filter by "real" here?
      cmds <- lapply(self$plan(target_name), function(i)
                     target_run_fake(self$get_target(i), for_script=TRUE))

      src <- c(unlist(pkgs),
               unlist(srcs),
               unlist(cmds))
      class(src) <- "maker_script"
      src
    },

    load_sources=function() {
      if (self$is_interactive()) {
        if (!identical(self$hash, hash_object(self$interactive))) {
          ## TODO: Can't paint here because paint is not defined.
          ## self$print_message("READ", "", "# reloading makerfile")
          self$reload()
        }
      } else {
        if (!identical(hash_files(names(self$hash)), self$hash)) {
          self$print_message("READ", "", "# reloading makerfile")
          self$reload()
        }
      }

      if (!self$store$env$is_current()) {
        self$print_message("READ", "", "# loading sources")
        self$store$env$reload(TRUE)
      }

      if (!is.null(self$envir)) {
        maker_reload_active_bindings(self, "source")
      }
    },

    update=function(target_name, dry_run=FALSE, force=FALSE,
      quiet_target=self$verbose$quiet_target, check=NULL,
      return_target=TRUE) {
      #
      target <- self$get_target(target_name)
      current <- !force && is_current(target, self$store, check)

      skip <- isTRUE(target$implicit)
      ## skip <- status == "" && target$type == "file" && is.null(target$rule)
      if (!skip) {
        status <- if (current) "OK" else target$status_string
        cmd <- if (current) NULL else target_run_fake(target)
        style <- if (is.null(target$chain_parent)) "square" else "curly"
        self$print_message(status, target_name, cmd, style)
      }

      if (!dry_run) {
        if (!current) {
          ## See #12 - targets can specify conditional packages, and
          ## we load them, but also unload them afterwards (including
          ## dependencies).  This does not leave packages loaded for
          ## dependent taragets though.
          extra <- load_extra_packages(target$packages)
          ret <- target_build(target, self$store, quiet=quiet_target)
          unload_extra_packages(extra)
          invisible(ret)
        } else if (return_target) {
          invisible(target_get(target, self$store))
        }
      }
    },

    plan=function(target_name=NULL, dependencies_only=FALSE) {
      if (is.null(target_name)) {
        target_name <- self$target_default()
      }
      graph <- self$dependency_graph()
      dependencies(target_name, graph, dependencies_only)
    },

    status=function(target_name=NULL) {
      if (is.null(target_name)) {
        target_name <- self$target_default()
      }
      graph <- self$dependency_graph()
      status(target_name, graph, self)
    },

    print_message=function(status, target_name, cmd=NULL, style="square") {
      verbose <- self$verbose
      if (!verbose$print_progress ||
          !verbose$print_noop && status %in% c("", "OK")) {
        return()
      } else if (!verbose$print_command) {
        cmd <- NULL
      }
      paint <- private$fmt$p$paint
      col <- status_colour(status)
      status <- brackets(paint(sprintf("%5s", status), col), style)
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
                       private$fmt$p$paint(cmd, "grey"))
      }
      message(str)
    },

    expire=function(target_name, recursive=FALSE) {
      if (recursive) {
        graph <- self$dependency_graph()
        for (t in dependencies(target_name, graph)) {
          self$expire(t)
        }
      } else {
        self$store$db$del(target_name, missing_ok=TRUE)
      }
    },

    archive_export=function(target_name, recursive=TRUE, filename="maker.zip") {
      archive_export_maker(self, target_name, recursive, filename)
    },

    ## TODO: Provide candidate set of targets to import?
    ## TODO: Import files/objects?
    archive_import=function(filename) {
      archive_import_maker(self, filename)
    },

    remove_targets=function(target_names, chain=TRUE) {
      for (t in target_names) {
        self$remove_target(t, chain)
      }
    },

    remove_target=function(target_name, chain=TRUE) {
      target <- self$get_target(target_name)
      if (chain && !is.null(target$chain_kids)) {
        chain_names <- dependency_names(target$chain_kids)
        self$remove_targets(chain_names, chain=FALSE)
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
      self$print_message(status, target_name, cmd, "round")
    },

    has_target=function(target_name) {
      target_name %in% names(self$targets)
    },

    get_target=function(target_name) {
      if (!self$has_target(target_name)) {
        stop("No such target ", target_name)
      }
      self$targets[[target_name]]
    },

    get_targets=function(target_names) {
      if (!all(self$has_target(target_names))) {
        stop("No such target ",
             paste(setdiff(target_names, names(self$targets)),
                   collapse=", "))
      }
      self$targets[target_names]
    },

    ## TODO: Filter chained targets from this set?
    get_targets_by_type=function(types) {
      filter_targets_by_type(self$targets, types)
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

    target_names=function(all=FALSE) {
      if (!all) {
        ok <- vlapply(self$targets, function(x) is.null(x$chain_parent))
        names(self$targets[ok])
      } else {
        names(self$targets)
      }
    },

    target_default=function() {
      if (is.null(self$default)) {
        stop(self$file,
             " does not define 'target_default' or have target 'all'")
      }
      self$default
    },

    ## Wrappers around the *object* store.  Not sure about the other
    ## stores, really.
    ls=function() {
      self$store$objects$ls()
    },
    get=function(name) {
      self$store$objects$get(name)
    },
    export=function(names, envir=.GlobalEnv) {
      self$store$objects$export(names, envir)
    },

    ## These two are really only used in the tests.
    is_current=function(target_name, check=NULL) {
      is_current(self$get_target(target_name), self$store, check)
    },
    dependency_status=function(target_name, missing_ok=FALSE, check=NULL) {
      dependency_status(self$get_target(target_name),
                        self$store, missing_ok, check)
    },

    build=function(target_name, quiet_target=self$verbose$quiet_target) {
      target_build(self$get_target(target_name), self$store, quiet_target)
    },

    dependency_graph=function() {
      g <- lapply(self$targets, function(t) t$depends_name)
      topological_sort(g)
    },

    ## Utilities:
    gitignore=function() {
      utility_gitignore(self)
    },

    install_packages=function() {
      utility_install_packages(self)
    },

    diagram=function() {
      diagram(self)
    }
  ),

  active=list(
    add=function(value) {
      if (missing(value)) {
        message("Pass in libary/source/target calls here")
      } else if (!self$is_interactive()) {
        stop("Cannot add packages when not running in interactive mode")
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
    fmt=NULL,

    initialize_cleanup_targets=function() {
      targets <- lapply(cleanup_target_names(), make_target_cleanup, self)
      self$add_targets(targets, force=TRUE)
    },

    ## NOTE: The logic here seems remarkably clumsy.
    initialize_default_target=function(default) {
      if (is.null(default)) {
        if ("all" %in% self$target_names()) {
          self$default <- "all"
        }
      } else {
        assert_scalar_character(default, "target_default")
        if (!(default %in% self$target_names())) {
          stop(sprintf("Default target %s not found in makerfile",
                       default))
        }
        self$default <- default
      }
    },

    initialize_targets_activate=function() {
      ## Add all targets that exist only as part of a chain.
      chain_kids <- unlist(lapply(self$targets, "[[", "chain_kids"),
                           FALSE)
      if (length(chain_kids) > 0L) {
        self$add_targets(chain_kids)
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
        max_cmd_width=width - (w0 + 1 + target_width + 4),
        p=painter$new(interactive()))
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
  .R6_maker$new(maker_file, verbose=verbose, envir=envir)
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
         DEL="red",
         UTIL="darkorchid3",
         READ="yellow",
         PLOT="dodgerblue2",
         KNIT="hotpink",
         MAKE="deepskyblue",
         ENV="deepskyblue",
         "-----"="grey",
         NULL)
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

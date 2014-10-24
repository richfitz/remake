##' The actual maker object to interact with.
##' @title Main maker object
##' @export
##' @importFrom R6 R6Class
maker <- R6Class(
  "maker",
  public=list(
    file=NULL,
    hash=NULL,
    path=NULL,
    store=NULL,
    plot_options=NULL,
    targets=NULL,
    ## NOTE: *verbose* applies to maker, while *quiet_target* applies
    ## to targets.  I might move to quiet here, instead.
    verbose=NULL,
    quiet_target=NULL,
    default=NULL,

    initialize=function(maker_file="maker.yml", path=".",
      verbose=TRUE, quiet_target=NULL) {
      self$file <- maker_file
      self$path <- path
      self$verbose <- verbose
      self$quiet_target <- quiet_target
      self$reload()
    },

    reload=function() {
      self$store <- store$new(self$path)
      config <- read_maker_file(self$file)
      self$hash <- config$hash
      self$plot_options <- config$plot_options
      self$targets <- NULL
      self$add_targets(config$targets)
      private$initialize_cleanup_targets()
      private$initialize_targets_activate()
      private$check_rule_target_clash()
      private$initialize_default_target(config$target_default)
      private$initialize_utility_targets() # last; nothing depends on these
      private$initialize_message_format()
      self$store$env <- managed_environment$new(config$packages, config$sources)
    },

    make=function(target_names=NULL, ...) {
      if (is.null(target_names)) {
        target_names <- self$target_default()
      }
      for (t in target_names) {
        if (length(target_names) > 1L) {
          self$print_message("MAKE", t, style="angle")
        }
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
      deps <- filter_targets_by_type(t$depends, "object")
      deps_name <- sapply(deps, function(x) x$name)

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
      force_all=FALSE, quiet_target=self$quiet_target, check=NULL,
      dependencies_only=FALSE) {
      #
      ## NOTE: Not 100% sure about this.  The "install_packages"
      ## target requires that the sources are not loaded before it is
      ## run, because it exists to install required packages.  So it
      ## needs to be picked up here specially.
      if (target_name != "install_packages") {
        self$load_sources()
      }
      graph <- self$dependency_graph()
      plan <- dependencies(target_name, graph, dependencies_only)
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
      ## This is repeated from make()
      graph <- self$dependency_graph()
      plan <- dependencies(target_name, graph)

      pkgs <- lapply(self$store$env$packages,
                     function(x) sprintf('library("%s")', x))
      srcs <- lapply(self$store$env$sources,
                     function(x) sprintf('source("%s")', x))
      ## Probably best to filter by "real" here?
      cmds <- lapply(plan, function(i)
                     self$get_target(i)$run_fake(for_script=TRUE))

      src <- c(unlist(pkgs),
               unlist(srcs),
               unlist(cmds))
      class(src) <- "maker_script"
      src
    },

    script_entry=function(target_name) {
      target <- self$get_target(target_name)$run_fake()
      target$run_fake()
    },

    load_sources=function() {
      if (!identical(hash_files(names(self$hash)), self$hash)) {
        self$print_message("READ", "", "# reloading makerfile")
        self$reload()
      }

      if (!self$store$env$current()) {
        self$print_message("READ", "", "# loading sources")
        self$store$env$reload()
      }
    },

    update=function(target_name, dry_run=FALSE, force=FALSE,
      quiet_target=self$quiet_target, check=NULL, return_target=TRUE) {
      #
      target <- self$get_target(target_name)
      current <- !force && target$is_current(check)

      status <- target$status_string(current)
      cmd <- if (current) NULL else target$run_fake()
      style <- if (is.null(target$chain_parent)) "square" else "curly"
      self$print_message(status, target_name, cmd, style)

      if (!dry_run) {
        if (!current) {
          ## See #12 - targets can specify conditional packages, and
          ## we load them, but also unload them afterwards (including
          ## dependencies).  This does not leave packages loaded for
          ## dependent taragets though.
          extra <- load_extra_packages(target$packages)
          ret <- target$build(quiet=quiet_target)
          unload_extra_packages(extra)
          invisible(ret)
        } else if (return_target) {
          invisible(target$get())
        }
      }
    },

    print_message=function(status, target_name, cmd=NULL, style="square") {
      paint <- private$fmt$p$paint
      col <- status_colour(status)
      status <- brackets(paint(sprintf("%5s", status), col), style)
      if (!is.null(cmd)) {
        w_extra <- max(0, nchar(target_name) - private$fmt$target_width)
        cmd <- abbreviate(cmd, private$fmt$max_cmd_width - w_extra)
      }
      if (is.null(cmd)) {
        str <- sprintf(private$fmt$no_cmd, status, target_name)
      } else {
        str <- sprintf(private$fmt$with_cmd, status, target_name,
                       private$fmt$p$paint(cmd, "grey"))
      }
      if (self$verbose) {
        message(str)
      }
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
      maker_archive_export(self, target_name, recursive, filename)
    },

    ## TODO: Provide candidate set of targets to import?
    ## TODO: Import files/objects?
    archive_import=function(filename) {
      maker_archive_import(self, filename)
    },

    remove_targets=function(target_names, chain=TRUE) {
      for (t in target_names) {
        self$remove_target(t, chain)
      }
    },

    remove_target=function(target_name, chain=TRUE) {
      target <- self$get_target(target_name)
      if (chain && !is.null(target$chain)) {
        chain_names <- sapply(target$chain, function(x) x$name)
        self$remove_targets(chain_names, chain=FALSE)
      }

      did_remove <- target$del(missing_ok=TRUE)
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

    add_targets=function(x, force=FALSE, activate=FALSE) {
      if (!all(sapply(x, inherits, "target_base"))) {
        stop("All elements must be targets")
      }
      target_names <- vapply(x, "[[", character(1), "name")
      if (any(duplicated(target_names))) {
        stop("All target names must be unique")
      }
      if (any(target_names %in% self$target_names())) {
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
      if (activate) {
        for (t in x) {
          t$activate(self)
        }
      }
    },

    ## TODO:
    ## * all=FALSE (excludes chain jobs, and/or all rules starting with
    ##   a period)
    ## * type=whatever
    target_names=function(all=FALSE) {
      if (!all) {
        ok <- as.logical(sapply(self$targets, function(x)
                                is.null(x$chain_parent)))
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

    ## Things that just pass through to the targets:
    is_current=function(target_name, check=NULL) {
      self$get_target(target_name)$is_current(check)
    },
    dependency_status=function(target_name, missing_ok=FALSE, check=NULL) {
      self$get_target(target_name)$dependency_status(missing_ok, check)
    },
    build=function(target_name, quiet_target=self$quiet_target) {
      self$get_target(target_name)$build(quiet=quiet_target)
    },

    dependency_graph=function() {
      targets <- self$target_names(all=TRUE)
      g <- lapply(targets, function(t) self$get_target(t)$dependencies())
      names(g) <- targets
      topological_sort(g)
    },

    diagram=function() {
      diagram(self)
    }
    ),
  private=list(
    fmt=NULL,

    initialize_cleanup_targets=function() {
      levels <- cleanup_target_names()
      targets <- list()
      for (i in seq_along(levels)) {
        target_name <- levels[[i]]
        if (target_name %in% self$target_names()) {
          depends <- self$get_target(target_name)$depends
          command <- self$get_target(target_name)$command
        } else {
          depends <- command <- NULL
        }
        if (i > 1L) {
          depends <- c(depends, list(levels[[i - 1L]]))
        }
        ## TODO: new function 'make_target_cleanup'?
        targets[[i]] <- make_target(target_name,
                                    list(command=command, depends=depends),
                                    "cleanup")
      }
      self$add_targets(targets, force=TRUE)
    },

    initialize_utility_targets=function() {
      add <- list(target_utility$new("install_packages",
                                     utility_install_packages, self),
                  target_utility$new("gitignore", utility_gitignore, self))
      self$add_targets(add)
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
      for (t in self$targets) {
        t$activate(self)
      }
    },

    initialize_message_format=function() {
      width <- getOption("width")
      w0 <- 10 # nchar("[ BUILD ] ")
      target_width <- min(max(nchar(self$target_names())) + w0,
                      ceiling(width / 2))
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
                 "plot_options",
                 "target_default", "targets"))

  dat$hash <- hash_files(filename, named=TRUE)

  if (length(seen) > 0L) { # an included file
    dat$target_default <- NULL
  }

  dat$packages <- with_default(dat$packages, character(0))
  dat$sources  <- with_default(dat$sources,  character(0))

  ## TODO: checking plot options (as done by make_target) will have to
  ## wait until after all includes have been read (i.e. at activate)
  ## once includes are supported.
  if (!is.null(dat$plot_options)) {
    assert_named_list(dat$plot_options)
    for (i in names(dat$plot_options)) {
      assert_named_list(dat$plot_options[[i]],
                        name=paste("plot_options: ", i))
    }
  }

  dat$targets <- lnapply(dat$targets, make_target)

  if (!is.null(dat$include)) {
    assert_character(dat$include)
    ## TODO: Even after sorting out main file restriction, this one
    ## may need some work. Could rewrite file-based rules to adjust
    ## relative paths, or leave relative paths going against the main
    ## file.  Not sure what the right answer here is, so requiring new
    ## files to be in the current working directory.
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

      if ("all" %in% names(dat_sub$targets)) {
        warning(f, " contains target 'all', which I am removing")
        dat_sub$targets$all <- NULL
      }

      ## TODO: This will be a repeated pattern for plot_options
      ## TODO: Should track which files have duplicates
      dups <- intersect(names(dat_sub$targets), names(dat$targets))
      if (length(dups) > 0L) {
        ## This will throw an error later on, but a warning here will
        ## make that easier to diagnose.
        warning(sprintf("%s contains duplicate targets %s",
                        f, paste(dups, collapse=", ")))
      }
      dat$targets  <- c(dat$targets, dat_sub$targets)

      ## TODO :THis is easy to add, but the logic around duplicates
      ## will proliferate unpleasantly.  Fix this for targets *first*,
      ## and then add this complication.
      if ("plot_options" %in% names(dat_sub)) {
        stop("plot_options in included makerfiles not yet supported")
      }
    }
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
         KNIT="hotpink",
         MAKE="deepskyblue",
         ENV="deepskyblue",
         NULL)
}

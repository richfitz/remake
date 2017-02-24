## New API functions.  Things in here will be the most stable,
## ideally.

######################################################################
## Main functionality:
######################################################################

##' Run remake to build one or more targets.
##' @title Make one or more targets
##' @param target_names Character vector of names of targets to build,
##' or `NULL` to build the default target (if specified in the
##' remakefile).
##' @param ... Additional future arguments, ignored for now.
##' Practically this means that all other arguments must be specified
##' by full name.
##' @param verbose Controls whether remake is verbose or not.  The
##' default (`TRUE`) prints out the name of each
##' target as it is built/checked.  This argument is passed to
##' [remake_verbose()]; valid options are `TRUE`,
##' `FALSE` and also the result of calling
##' [remake_verbose()].
##' @param allow_missing_packages Allow missing packages when loading
##' remake file?
##' @param remake_file Name of the remakefile (by default
##' `remake.yml`).
##' @export
make <- function(target_names=NULL, ...,
                 verbose=TRUE,
                 allow_missing_packages=FALSE,
                 remake_file="remake.yml") {
  obj <- remake(remake_file, verbose,
                allow_missing_packages=allow_missing_packages)
  remake_make(obj, target_names)
}

##' Create a simple standalone script from a remake file.
##' @title Write standalone script to make targets
##' @param target_names Character vector of names of targets to build,
##' or `NULL` to build the default target (if specified in the
##' remakefile).
##' @param verbose Be verbose when loading the remake file?
##' @param filename A filename to save the resulting script into.  If
##' `NULL` (the default) then an a character vector is returned
##' that can be inspected.  It can also be sourced without writing to
##' file using [source_character()].
##' @param remake_file Name of the remakefile (by default
##' `remake.yml`).  This is passed to `remake()`.
##' @param ... Additional future arguments, ignored for now.
##' Practically this means that all other arguments must be specified
##' by full name.
##' @export
make_script <- function(target_names=NULL,
                        verbose=FALSE,
                        filename=NULL,
                        remake_file="remake.yml") {
  obj <- remake(remake_file, verbose=verbose, load_sources=FALSE)
  scr <- remake_script(obj, target_names)
  if (is.null(filename)) {
    scr
  } else {
    writeLines(scr, filename)
    invisible(scr)
  }
}

##' Install missing packages
##'
##' No version comparison is done - see packrat for a more complete
##' package management solution, though in my initial experiments it
##' does not play that nicely with remake.
##' @title Install missing packages
##' @param remake_file Name of the remakefile to look for the list of
##' required packages
##' @param instructions Rather than install anything, prints
##' instructions on how to install missing things
##' @param missing_only If `FALSE`, install \emph{everything},
##' rather than just missing packages.  This might be useful to set to
##' `TRUE` in conjunction with `instructions=TRUE` to
##' generate a full list to install.
##' @param skip_target_packages Skip packages that are mentioned only
##' in targets?
##' @export
##' @author Rich FitzJohn
install_missing_packages <- function(remake_file="remake.yml",
                                     instructions=FALSE,
                                     missing_only=TRUE,
                                     skip_target_packages=FALSE) {
  ## TODO: should this be remake(remake_file, verbose, load_sources=FALSE)?
  dat <- read_remake_file(remake_file)
  packages <- with_default(dat$packages, character(0))
  if (!skip_target_packages) {
    packages <- c(packages, target_packages(dat))
  }
  package_sources <- read_remake_packages("remake_sources.yml")
  ret <- install_packages(packages,
                          instructions=instructions,
                          missing_only=missing_only,
                          package_sources=package_sources)
  if (instructions) {
    message(ret)
  }
  invisible(packages)
}

##' `create_bindings()` loads bindings from `remake` into the global environment
##' ([`.GlobalEnv`][.GlobalEnv]).  The resulting objects are "active
##' bindings" that when accessed will fetch an object.
##' You still need to use [make()] to update targets.
##' @title Load remake bindings into the global environment
##' @param remake_file Name of the remakefile to read.  By default
##' `"remake.yml"`.
##' @export
create_bindings <- function(remake_file="remake.yml") {
  ## TODO: Are these really the best names?  They're explicit, but
  ## they're not very pleasant.
  ##
  ## TODO: Option to just set up the source ones so that things can be
  ## run easily.
  global_active_bindings$create_bindings(remake_file)
}
##' @description
##' Conversely, `delete_bindings()` undoes this and deletes the
##' bindings that `remake` instantiated before.
##' @export
##' @rdname create_bindings
delete_bindings <- function(remake_file="remake.yml") {
  global_active_bindings$delete_bindings(remake_file)
}

##' Plot the graph that remake generates.
##'
##' This is really just a placeholder, but I want this here early as
##' an indication of where the package is headed.  Plus this is
##' something I have always wanted in `make`.  Current version is
##' not tunable on purpose.
##' @title Make a figure with the dependency graph
##' @param ... Additional arguments that control formatting but aren't
##' documented and are subject to change.
##' @param remake_file Name of remake file (default is
##' `remake.yml`).
##' @return An htmlwidgets object, which are embeddable in all sorts
##' of things and print to the screen and seem very fancy.  See the
##' `grViz` help for more information.
##' @export
diagram <- function(..., remake_file="remake.yml") {
  ## TODO: Take a target name here so we can get the tree filtered to
  ## a set of targets.
  ##
  ## TODO: Why is this not taking verbose, and using load_sources=FALSE?
  obj <- remake(remake_file)
  str <- remake_diagram_command(obj)
  DiagrammeR::grViz(str)
}

######################################################################
## Support functions
######################################################################

##' Helper function to set options for verbosity.
##'
##' The first four options have a natural nesting: setting
##' `progress=FALSE` prevents printing any progress information,
##' so the value of `noop`, `command` and
##' `command_abbreviate` does not matter.  Similarly, setting
##' `command=FALSE` means that `command_abbreviate` does not
##' matter.
##' @title Control remake verbosity
##' @param verbose Print progress at each step that remake does
##' something.
##' @param noop Print progress for steps that are non-operations, such
##' as targets that need nothing done to them.  Setting this to
##' `FALSE` is useful for very large projects.
##' @param command Print the command along with the progress
##' information?  This is only printed when remake actually runs
##' something.
##' @param command_abbreviate Abbreviate the command information so
##' that it fits on one line.  If `FALSE` then the command will
##' be allowed to run on for as many lines as required.
##' @param target Print information that the target produces (via
##' [message()], [cat()] or [print()]).  If `FALSE` then
##' these messages will be suppressed.
##' @export
remake_verbose <- function(verbose=getOption("remake.verbose", TRUE),
                          noop=getOption("remake.verbose.noop", TRUE),
                          command=getOption("remake.verbose.command", TRUE),
                          command_abbreviate=getOption("remake.verbose.command.abbreviate", TRUE),
                          target=NULL) {
  if (inherits(verbose, "remake_verbose")) {
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
              class="remake_verbose")
  }
}

##' Install running script to a local directory.  This directory
##' should be on the `$PATH`.  Once this has been done, you can
##' run remake with `remake`.  See `remake --help` for more
##' information, or a summary below.
##'
##' The installed script is just a wrapper to an internal remake
##' function, designed so that the wrapper script does not need to be
##' installed after upgrading remake.
##'
##' Because of the design of [Rscript], the helper script
##' loads the `methods` package: in my experience many things just do not
##' work without that package loaded and Rscript does not load it by
##' default (especially in conjunction with `::`).
##'
##' This `remake` script accepts standard unix-style command-line
##' options to drive a subset of remake's functionality.  Supported
##' featues are:
##'
##' \itemize{
##' \item Running [make()], as `remake target1 [target2 ...]`.
##'  If targets are omitted the default target will be
##' used.
##'
##' \item listing targets, as `--list-targets`, which calls
##' [list_targets()]
##'
##' \item generating a script with `-s` or `--script`
##' (printing to standard output) or `--script-file` prints to a
##' file.
##'
##' \item return the version, as `-v` or `--version`,
##' returning `packageVersion("remake")`
##' }
##'
##' Additionally, the file used can be selected by using `-f` or
##' `--file` (following `make`'s convention), and remake can
##' be run with `verbose=FALSE` by passing in `-q` or
##' `--quiet`.
##'
##' @title Install running script.
##' @param destination_directory Directory to install `remake` to.
##' Should be on your path, though the current directory may be useful
##' too.  The file will be installed as \code{file.path(dest,
##' "remake")}
##' @param overwrite Overwrite an existing file?
##' @export
install_remake <- function(destination_directory, overwrite=FALSE) {
  if (!file.exists(destination_directory) ||
      !is_directory(destination_directory)) {
    stop("Destination must be an existing directory")
  }
  file <- file.path(destination_directory, "remake")
  if (file.exists(file) && !overwrite) {
    stop(sprintf("File %s already exists", file))
  }
  code <- c("#!/usr/bin/env Rscript", "library(methods)", "remake:::main()")
  writeLines(code, file)
  Sys.chmod(file, "0755")
}

##' Returns the vector of \emph{default} file extensions.  If a target
##' ends in one of these, then it will be considered a file, rather
##' than an object.
##'
##' To include \emph{additional} file extensions, include them in the
##' yaml like (at the top level):
##'
##' \preformatted{
##' file_extensions: ["phy", "tre"]
##' }
##'
##' Any number of extensions can be listed.  Don't use a leading
##' period (it will be dropped).  The union of file extensions listed
##' here and in `file_extensions()` will be used, so if you
##' accidently include a default extension (or if one is included in a
##' future remake version) it is no problem.
##'
##' @title Vector of file extensions
##' @export
file_extensions <- function() {
  c(# Data
    "csv", "tsv", "xls", "xlsx", "rds", "rda", "rdata",
    # Free form
    "txt", "log", "yml", "yaml", "xml",
    # Text
    "md", "tex", "r", "rmd", "rnw", "html", "htm", "bib",
    # Graphics
    "jpg", "jpeg", "png", "pdf", "eps", "ps", "bmp", "tiff", "svg",
    # Archives
    "zip", "gz", "tar", "bz2")
}

##' Convenience function for sourcing a remake script (or other
##' arbitrary code) that is present as a character vector rather than
##' saved in a file.  This just takes
##' care of writing the character vector to a temporary file and
##' running R's [source()] over it.  It will also source
##' other arbitrary sets of R code that are character vectors rather
##' than files.
##' @title Source a remake script
##' @param str A character vector containing code to be sourced.  Each
##' element of the vector will be treated as a separate line.
##' @param envir An environment to source into (by default the global
##' environment).
##' @param rewrite_source Because calls to `source()` within a
##' script will still be evaluated in the global environment, so this
##' may have side-effects when running in a non-global
##' environment.  Setting `rewrite_source=TRUE` (the default)
##' attempts to rewrite top-level calls to `source()` to source
##' locally.  This is likely error prone but the current
##' implementation matches the way that `make_script()` produces
##' calls to `source()`.
##' @return The environment into which the code is sourced,
##' invisibly.  This is primarily useful when used as
##' `source_remake_script(script, envir=new.env())`, as the
##' environment created in the call is returned.
##' @export
##' @examples
##' str <- c("x <- runif(10)",
##'          "y <- runif(10)",
##'          "plot(x, y)")
##' e <- source_character(str, envir=new.env())
##' ls(e) # x, y
source_character <- function(str, envir=.GlobalEnv, rewrite_source=TRUE) {
  assert_character(str)
  if (!identical(envir, .GlobalEnv) && rewrite_source) {
    ## TODO: should do this with parse or the codeTools stuff
    ## probably.  For now I'm trying to match how this is written
    ## out.  It's not beautiful for sure.  We would not want to
    ## rewrite a line where source has been redefined of course!
    str <- sub('^source\\("(.*)"\\)$', 'source("\\1", local=TRUE)', str)
    ## This way runs with R's parser, but I'm not convinced it's much
    ## better:
    ## f <- function(x) {
    ##   if (length(x) == 2L && identical(x[[1]], as.name("source"))) {
    ##     x <- call("source", x[[2]], local=FALSE)
    ##   }
    ##   deparse(x)
    ## }
    ## str <- vcapply(parse(text=str), f)
  }
  dest <- tempfile()
  writeLines(str, dest)
  on.exit(file_remove(dest))
  sys.source(dest, envir)
  invisible(envir)
}

##' List targets contained within a remakefile
##'
##' Do not rely on argument ordering here: please use argument names.
##' @title List targets
##' @param remake_file Name of the remakefile to read (by default
##' `remake.yml`)
##' @param type Type of target to return.  May be `fake`,
##' `object`, `file` or `cleanup`.  Eventually subtypes
##' will be supported (knitr and plot targets) but this is not
##' possible yet.
##' @param include_implicit_files Logical scalar indicating if
##' implicit targets should be included.
##' @param include_cleanup_targets Logical scalar indicating if cleanup
##' targets (which are automatically generated) should be included.
##' @return A character vector containing names of targets.
##' @export
list_targets <- function(remake_file="remake.yml",
                         type=NULL,
                         include_implicit_files=FALSE,
                         include_cleanup_targets=FALSE) {
  obj <- remake(remake_file, verbose=FALSE, load_sources=FALSE)
  remake_list_targets(obj,
                      type,
                      include_implicit_files,
                      include_cleanup_targets)
}
##' @rdname list_targets
##' @param target_names Names of targets to list dependencies of (for
##' `list_dependencies`).  These dependencies will be filtered as
##' for `list_targets`.  Dependencies are listed in topological
##' order: targets have no dependencies that occur later than them in
##' the vector.
##' @export
list_dependencies <- function(target_names,
                              type=NULL,
                              include_implicit_files=FALSE,
                              include_cleanup_targets=FALSE,
                              remake_file="remake.yml") {
  obj <- remake(remake_file, verbose=FALSE, load_sources=FALSE)
  remake_list_dependencies(obj, target_names,
                           type,
                           include_implicit_files,
                           include_cleanup_targets)
}

##' Determine if one or more targets are "current" or not.  A target
##' is current if (1) it exists, (2) its immediate dependencies are
##' unchanged since it was last built and (3) its code is unchanged
##' since it was last built.
##'
##' Note that this function does not check all the way down the
##' dependency tree; so if A depends on B and B depends on C, A may be
##' current with respect to B but B may be out of date with respect to
##' C.  Therefore running make()] would trigger building B, which
##' \emph{may} imply rebuilding A.
##' @title Determine if targets are current
##' @param target_names Names of one or more targets to check
##' @param check What to check.  By default (`check=NULL`) this
##' will check both code and dependencies unless overridden in the
##' makerfile.  Other valid options are `"exists"` (current if
##' the target exists), `"depends"` (current if exists and
##' dependencies unchanged), `"code"` (current if exists and code
##' unchanged) or `"all"` (current if exists and both
##' dependencies and code unchanged).
##' @param verbose Be verbose when loading remake file?  Default is
##' `FALSE`.
##' @param allow_missing_packages Allow missing packages when loading
##' remake file?
##' @param remake_file Name of the remakefile (by default
##' `remake.yml`).
##' @return A logical vector the same length as `target_names`.
##' @export
is_current <- function(target_names, check=NULL,
                       verbose=FALSE, allow_missing_packages=FALSE,
                       remake_file="remake.yml") {
  obj <- remake(remake_file, verbose=verbose,
                allow_missing_packages=allow_missing_packages)
  remake_is_current(obj, target_names, check)
}

##' Attempts to add targets that remake will generate to your
##' `.gitignore`.  If the file already exists, then the files
##' will be added (if not already present), otherwise a file will be
##' created.  If `check_git` is `TRUE` (the default) then we
##' attempt to check with `git` to see if the files are already
##' ignored by \emph{any} gitignore (including a system-specific
##' global gitignore) and only files that are not already excluded
##' will be added.  Probably after running this function you will want
##' to do some editing.
##' @title Automatically generate .gitignore
##' @param remake_file Name of the remake file to use, by default
##' `remake.yml`.
##' @param check_git Use the output of `git check-ignore` to
##' determine which files are already ignored?  This is only done if
##' git is detected to be installed and if the working directory
##' appears to be in a git repository.
##' @param dry_run Don't modify the .gitignore, but instead return a
##' character vector of what \emph{would} be added.
##' @export
auto_gitignore <- function(remake_file="remake.yml", check_git=TRUE,
                           dry_run=FALSE) {
  files <- c(".remake", list_targets(remake_file, type="file"))
  if (check_git && git_exists()) {
    ignored <- git_ignores(files)
    files <- files[!ignored]
  } else if (file.exists(".gitignore")) {
    curr <- readLines(".gitignore")
    files <- setdiff(files, strip_whitespace(curr))
  }
  if (!dry_run && length(files) > 0) {
    append_lines(files, ".gitignore")
  }

  if (dry_run) {
    files
  } else {
    invisible(files)
  }
}

##' Construct an environment with remake target products, useful for
##' debugging.  Once you have an environment, you can
##' [attach()] it (yes, it \emph{is} useful for something,
##' but be careful to [detach()] later), extract elements or browse
##' it.
##' @title Construct environment
##' @param target_names Vector of target names to export.  If omitted,
##' then no targets are copied, though functions are still copied.
##' @param dependencies Should the dependencies of `target_names`
##' also be copied over?  Setting this to `TRUE` is equivalent to
##' `make_environment(list_dependencies(target_names, type="file"))`
##' but shorter to type.
##' @param copy_functions Should functions be directly copied into
##' the returned environment?  If `FALSE`, then the returned
##' environment has an environment with functions as its
##' \emph{parent}.  This is the same as the environment used by
##' `remake` so don't assign anything in here!  (This may change
##' if it ends up being a point of fragility.)
##' @param verbose Be verbose?
##' @param allow_missing_packages Allow missing packages when loading
##' remake file?
##' @param remake_file Remake file to use, by default
##' `remake.yml`.
##' @export
make_environment <- function(target_names=character(0),
                             dependencies=FALSE,
                             copy_functions=TRUE,
                             verbose=TRUE,
                             allow_missing_packages=FALSE,
                             remake_file="remake.yml") {
  obj <- remake(remake_file, verbose=verbose,
                allow_missing_packages=allow_missing_packages)
  remake_environment(obj, target_names,
                     dependencies=dependencies,
                     copy_functions=copy_functions)
}

##' Fetch the last computed value from the remake database.
##'
##' The last computed value would be returned invisibly by
##' [make()], but this function provides a way of accessing values
##' without ever triggering a rebuild.  As such, it's possible that the
##' target is not made, or is not current, so there are options for
##' controlling what to do in this case.
##'
##' It is an error to use this function with file targets (but see
##' [is_current()] for checking currentness) and
##' [fetch_archive()] for extracting files from archives.
##' @title Fetch last computed
##' @param target_name The name of a single target to fetch the value
##' of
##' @param require_current Logical indicating if the targets must be
##' up-to-date to be fetched.  If this is `TRUE` and the targets
##' are not up-to-date, then an error will be thrown.
##' @param verbose Be verbose when loading remake file?  Default is
##' `TRUE`.
##' @param allow_missing_packages Allow missing packages when loading
##' remake file?
##' @param remake_file Name of the remakefile (by default
##' `remake.yml`)
##' @return An R object.
##' @export
fetch <- function(target_name, require_current=FALSE,
                  verbose=TRUE,
                  allow_missing_packages=FALSE,
                  remake_file="remake.yml") {
  assert_scalar_character(target_name)
  obj <- remake(remake_file, verbose=verbose,
                load_sources=require_current,
                allow_missing_packages=allow_missing_packages)
  assert_has_targets(target_name, obj)
  if (obj$targets[[target_name]]$type != "object") {
    stop("Can only fetch object targets")
  }
  if (require_current) {
    if (!remake_is_current(obj, target_name)) {
      stop("Object is out of date")
    }
  } else {
    if (!remake_is_current(obj, target_name, "exists")) {
      stop("Object has not been made")
    }
  }
  ## NOTE: to get metadata instead, we'd return
  ##   obj$store$db$get(target_name)
  ## and allow files through in the type check.
  obj$store$objects$get(target_name)
}

##' Delete targets.  Deletes both file and object targets, and removes
##' their entries from the remake database.  Using
##' `make("clean")` should probably be the general way to clean
##' up, but this might be useful if you have specific objects to
##' delete.  While files can be deleted in this way, deleting in the
##' file system is also fine.
##'
##' This function ignores `cleanup_level` and will quite happily
##' delete things that have been flagged as `cleanup_level: purge` -
##' be careful using `dependencies=TRUE` as this will
##' delete all dependencies.  See [list_dependencies()] to
##' see what would be deleted.
##'
##' It is an error to try to delete a fake target (i.e., a target with
##' no rule but that exists to group other dependencies).  It is
##' \emph{not} an error to delete the \emph{dependencies} of such a
##' target.
##'
##' If run with `verbose=TRUE` `delete` will print
##' information about targets that are deleted with a `DEL` for
##' each deleted target and an empty string if the target is already
##' nonexistant.
##' @title Delete targets
##' @param target_names Names of targets to delete
##' @param dependencies Delete dependencies of the target too?  Use
##' with caution.
##' @param verbose Be verbose when loading the remake file and when
##' deleting targets.
##' @param remake_file Name of the remakefile (by default
##' `remake.yml`)
##' @export
delete <- function(target_names, dependencies=FALSE,
                   verbose=TRUE, remake_file="remake.yml") {
  assert_character(target_names)
  obj <- remake(remake_file, verbose=verbose, load_sources=FALSE)
  if (dependencies) {
    target_names <- remake_list_dependencies(obj, target_names,
                                             type=c("file", "object"))
  }
  for (t in target_names) {
    remake_remove_target(obj, t)
  }
}

##' Dump the contents of remake into an environment; by default the
##' global environment.  This is similar in effect to
##' [create_bindings()] but does not create links; instead a
##' copy of everything that remake has built, plus all functions
##' sources into remake, are \emph{copied} into the environment.
##' @title Dump remake contents to environment
##' @param envir Environment to copy into; by default the global environment.
##' @param verbose Be verbose when loading the remakefile
##' @param allow_missing_packages Allow missing packages when loading
##' remake file?
##' @param remake_file Name of the remakefile (by default
##' `remake.yml`)
##' @export
dump_environment <- function(envir=.GlobalEnv, verbose=TRUE,
                             allow_missing_packages=FALSE,
                             remake_file="remake.yml") {
  obj <- remake(remake_file, verbose=verbose, load_sources=TRUE,
                allow_missing_packages=allow_missing_packages)
  remake_dump_environment(obj, envir)
}

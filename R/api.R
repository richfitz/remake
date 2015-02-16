## New API functions.  Things in here will be the most stable,
## ideally.

######################################################################
## Main functionality:
######################################################################

##' @title Make one or more targets
##' @param target_names Character vector of names of targets to build,
##' or \code{NULL} to build the default target (if specified in the
##' remakefile).
##' @param ... Additional future arguments, ignored for now.
##' Practically this means that all other arguments must be specified
##' by full name.
##' @param verbose Controls whether remake is verbose or not.  By
##' default it is (\code{TRUE}), which prints out the name of each
##' target as it is built/checked.  This argument is passed to
##' \code{\link{remake_verbose}}; valid options are \code{TRUE},
##' \code{FALSE} and also the result of calling
##' \code{\link{remake_verbose}}.
##' @param remake_file Name of the remakefile (by default
##' \code{remake.yml}).
##' @export
make <- function(target_names=NULL, ...,
                 verbose=TRUE,
                 remake_file="remake.yml") {
  remake_make(remake(remake_file, verbose), target_names)
}

##' @title Write standalone script to make targets
##' @param target_names Character vector of names of targets to build,
##' or \code{NULL} to build the default target (if specified in the
##' remakefile).
##' @param verbose Be verbose when loading the remake file?
##' @param filename A filename to save the resulting script into.  If
##' \code{NULL} (the default) then an a character vector is returned
##' that can be inspected.  It can also be sourced without writing to
##' file using \code{\link{source_character}}.
##' @param remake_file Name of the remakefile (by default
##' \code{remake.yml}).  This is passed to \code{remake()}.
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
##' @param missing_only If \code{FALSE}, install \emph{everything},
##' rather than just missing packages.  This might be useful to set to
##' \code{TRUE} in conjunction with \code{instructions=TRUE} to
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
    packages <- c(packages,
                  unlist(lapply(dat$targets, function(x) x$packages)))
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

##' Load bindings from \code{remake} into the global environment
##' (\code{\link{.GlobalEnv}}.  The resulting objects are "active
##' bindings" that when accessed will trigger a build of an object.
##' Conversely, \code{delete_bindings} undoes this and deletes the
##' bindings that \code{remake} made in the first place.
##' @title Load remake bindings into the global environment
##' @param remake_file Name of the remakefile to read.  By default
##' \code{"remake.yml"}.
##' @export
create_bindings <- function(remake_file="remake.yml") {
  ## TODO: Perhaps filter through to export only some names?
  ## Definitely filter through and do not export chain targets!
  ##
  ## TODO: Are these really the best names?  They're explicit, but
  ## they're not very pleasant.
  global_active_bindings$create_bindings(remake_file)
}
##' @export
##' @rdname create_bindings
delete_bindings <- function(remake_file="remake.yml") {
  global_active_bindings$delete_bindings(remake_file)
}

##' Plot the graph that remake generates.
##'
##' This is really just a placeholder, but I want this here early as
##' an indication of where the package is headed.  Plus this is
##' something I have always wanted in \code{make}.  Current version is
##' not tunable on purpose.
##' @title Make a figure with the dependency graph
##' @param ... Additional arguments that control formatting but aren't
##' documented and are subject to change.
##' @param remake_file Name of remake file (default is
##' \code{remake.yml}).
##' @return An htmlwidgets object, which are embeddable in all sorts
##' of things and print to the screen and seem very fancy.  See the
##' \code{grViz} help for more information.
##' @export
diagram <- function(..., remake_file="remake.yml") {
  ## TODO: Take a target name here so we can get the tree filtered to
  ## a set of targets.
  ## TODO: Colour differently based on up-to-date-ness.
  obj <- remake(remake_file, load_sources=FALSE)
  str <- remake_diagram_command(obj, ...)
  DiagrammeR::grViz(str)
}

######################################################################
## Support functions
######################################################################

##' Helper function to set options for verbosity.
##'
##' The first four options have a natural nesting: setting
##' \code{progress=FALSE} prevents printing any progress information,
##' so the value of \code{noop}, \code{command} and
##' \code{command_abbreviate} does not matter.  Similarly, setting
##' \code{command=FALSE} means that \code{command_abbreviate} does not
##' matter.
##' @title Control remake verbosity
##' @param verbose Print progress at each step that remake does
##' something.
##' @param noop Print progress for steps that are non-operations, such
##' as targets that need nothing done to them.  Setting this to
##' \code{FALSE} is useful for very large projects.
##' @param command Print the command along with the progress
##' information?  This is only printed when remake actually runs
##' something.
##' @param command_abbreviate Abbreviate the command information so
##' that it fits on one line.  If \code{FALSE} then the command will
##' be allowed to run on for as many lines as required.
##' @param target Print information that the target produces (via
##' \code{message}, \code{cat} or \code{print}).  If \code{FALSE} then
##' these messages will be suppressed.
##' @export
remake_verbose <- function(verbose=getOption("remake.verbose", TRUE),
                          noop=getOption("remake.verbose.noop", TRUE),
                          command=getOption("remake.verbose.command", TRUE),
                          command_abbreviate=TRUE,
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
##' should be on the \code{$PATH}.  Once this has been done, you can
##' run remake with `remake`.  See `remake --help` for more
##' information, or a summary below.
##'
##' The installed script is just a wrapper to an internal remake
##' function, designed so that the wrapper script does not need to be
##' installed after upgrading remake.
##'
##' Because of the design of \code{\link{Rscript}}, the helper script
##' loads the method package: in my experience many things just do not
##' work without that package loaded and Rscript does not load it by
##' default (especially in conjunction with \code{::}).
##'
##' This \code{remake} script accepts standard unix-style command-line
##' options to drive a subset of remake's functionality.  Supported
##' featues are:
##'
##' \itemize{
##' \item Running \code{remake::make}, as \code{remake target1
##' [target2]}.  If targets are ommitted the default target will be
##' used.
##'
##' \item listing targets, as \code{--list-targets}, which calls
##' \code{\link{list_targets}}
##'
##' \item generating a script with \code{-s} or \code{--script}
##' (printing to standard output) or \code{--script-file} prints to a
##' file.
##'
##' \item return the version, as \code{-v} or \code{--version},
##' returning \code{packageVersion("remake")}
##' }
##'
##' Additionally, the file used can be selected by using \code{-f} or
##' \code{--file} (following \code{make}'s convention), and remake can
##' be run with \code{verbose=FALSE} by passing in \code{-q} or
##' \code{--quiet}.
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

##' Returns the vector of known file extensions.  If a target ends in
##' one of these, then it will be considered a file, rather than an
##' object.  In a future version, it might be possible to configure
##' additional extensions: please let me know if that would be useful.
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
##' running R's \code{\link{source}} over it.  It will also source
##' other arbitrary sets of R code that are character vectors rather
##' than files.
##' @title Source a remake script
##' @param str A character vector containing code to be sourced.  Each
##' element of the vector will be treated as a separate line.
##' @param envir An environment to source into (by default the global
##' environment).
##' @param rewrite_source Because calls to \code{source} within a
##' script will still be evaluated in the global environment, so this
##' may have side-effects when running in a non-global
##' environment.  Setting \code{rewrite_source=TRUE} (the default)
##' attempts to rewrite top-level calls to \code{source} to source
##' locally.  This is likely error prone but the current
##' implementation matches the way that \code{make_script} produces
##' calls to \code{source}.
##' @return The environment into which the code is sourced,
##' invisibly.  This is primarily useful when used as
##' \code{source_remake_script(script, envir=new.env())}, as the
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
##' \code{remake.yml})
##' @param type Type of target to return.  May be \code{fake},
##' \code{object}, \code{file} or \code{cleanup}.  Eventually subtypes
##' will be supported (knitr and plot targets) but this is not
##' possible yet.
##' @param include_implicit_files Logical scalar indicating if
##' implicit targets should be included.
##' @param include_cleanup_targets Logical scalar indicating if cleanup
##' targets (which are automatically generated) should be included.
##' @param include_chain_intermediates Logical scalar indicating if
##' chain intermediates (automatically generated with mangled names)
##' should be included.
##' @return A character vector containing names of targets.
##' @export
list_targets <- function(remake_file="remake.yml",
                         type=NULL,
                         include_implicit_files=FALSE,
                         include_cleanup_targets=FALSE,
                         include_chain_intermediates=FALSE) {
  obj <- remake(remake_file, verbose=FALSE, load_sources=FALSE)
  remake_list_targets(obj,
                      type,
                      include_implicit_files,
                      include_cleanup_targets,
                      include_chain_intermediates)
}
##' @rdname list_targets
##' @param target_names Names of targets to list dependencies of (for
##' \code{list_dependencies}).  These dependencies will be filtered as
##' for \code{list_targets}.  Dependencies are listed in topological
##' order: targets have no dependencies that occur later than them in
##' the vector.
##' @export
list_dependencies <- function(target_names,
                              type=NULL,
                              include_implicit_files=FALSE,
                              include_cleanup_targets=FALSE,
                              include_chain_intermediates=FALSE,
                              remake_file="remake.yml") {
  obj <- remake(remake_file, verbose=FALSE, load_sources=FALSE)
  remake_list_dependencies(obj, target_names,
                           type,
                           include_implicit_files,
                           include_cleanup_targets,
                           include_chain_intermediates)
}

##' Determine if one or more targets are "current" or not.  A target
##' is current if (1) it exists, (2) its immediate dependencies are
##' unchanged since it was last built and (3) its code is unchanged
##' since it was last built.
##'
##' Note that this function does not check all the way down the
##' dependency tree; so if A depends on B and B depends on C, A may be
##' current with respect to B but B may be out of date with respect to
##' C.  Therefore running \code{make} would trigger building B, which
##' \emph{may} imply rebuilding A.
##' @title Determine if targets are current
##' @param target_names Names of one or more targets to check
##' @param check What to check.  By default (\code{check=NULL}) this
##' will check both code and dependencies unless overridden in the
##' makerfile.  Other valid options are \code{"exists"} (current if
##' the target exists), \code{"depends"} (current if exists and
##' dependencies unchanged), \code{"code"} (current if exists and code
##' unchanged) or \code{"all"} (current if exists and both
##' dependencies and code unchanged).
##' @param verbose Be verbose when loading remake file?  Default is
##' \code{FALSE}.
##' @param remake_file Name of the remakefile (by default
##' \code{remake.yml}).
##' @return A logical vector the same length as \code{target_names}.
##' @export
is_current <- function(target_names, check=NULL,
                       verbose=FALSE, remake_file="remake.yml") {
  obj <- remake(remake_file, verbose=verbose)
  remake_is_current(obj, target_names, check)
}

##' Attempts to add targets that remake will generate to your
##' \code{.gitignore}.  If the file already exists, then the files
##' will be added (if not already present), otherwise a file will be
##' created.  If \code{check_git} is \code{TRUE} (the default) then we
##' attempt to check with \code{git} to see if the files are already
##' ignored by \emph{any} gitignore (including a system-specific
##' global gitignore) and only files that are not already excluded
##' will be added.  Probably after running this function you will want
##' to do some editing.
##' @title Automatically generate .gitignore
##' @param remake_file Name of the remake file to use, by default
##' \code{remake.yml}.
##' @param check_git Use the output of \code{git check-ignore} to
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
    ignored <- try(git_ignores(files))
    if (!inherits(ignored, "try-error")) {
      files <- files[!ignored]
    }
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
##' \code{\link{attach}} it (yes, it \emph{is} useful for something,
##' but be careful to \code{detach} later), extract elements or browse
##' it.
##' @title Construct environment
##' @param target_names Vector of target names to export.  If omitted,
##' then no targets are copied, though functions are still copied.
##' @param dependencies Should the dependenciesof \code{target_names}
##' also be copied over?  Setting this to \code{TRUE} is equivalent to
##' \code{make_environment(list_dependencies(target_names, type="file"))}
##' but shorter to type.
##' @param copy_functions Should functions be directly copied into
##' the retuned environment?  If \code{FALSE}, then the returned
##' environment has an environment with functions as its
##' \emph{parent}.  This is the same as the environment used by
##' \code{remake} so don't assign anything in here!  (This may change
##' if it ends up being a point of fragility.)
##' @param verbose Be verbose?
##' @param remake_file Remake file to use, by default
##' \code{remake.yml}.
##' @export
make_environment <- function(target_names=character(0),
                             dependencies=FALSE,
                             copy_functions=TRUE,
                             verbose=TRUE,
                             remake_file="remake.yml") {
  obj <- remake(remake_file, verbose=verbose)
  remake_environment(obj, target_names,
                     dependencies=dependencies,
                     copy_functions=copy_functions)
}

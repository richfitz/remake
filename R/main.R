##' The command line script, to be run outside of the package.  Don't
##' run this from within R (well, you can't actually).  This uses
##' \code{commandArgs} to pass along arguments to \code{\link{remake}}.
##' @title Command line interface to remake
##' @param args Not yet documented
##' @export
main <- function(args=commandArgs(TRUE)) {
  if (interactive()) {
    stop("This is not meant to be used from an R session!")
  }
  parser <- optparse::OptionParser(option_list=remake_options())
  args <- optparse::parse_args(parser, args, positional_arguments=TRUE)
  opts <- args$options
  if (opts$version) {
    print_version()
    return(invisible())
  }
  m <- remake(opts$file, verbose=!opts$quiet)
  if (opts$print_targets) {
    print_targets(m)
    return(invisible())
  }
  targets <- args$args
  if (length(targets) == 0L) {
    targets <- NULL
  }
  if (opts$script) {
    if (length(targets) > 1L) {
      stop("Expected exactly one target with --script")
    }
    writeLines(remake_script(m, targets))
  } else {
    remake_make(m, targets, dry_run=opts$dry_run)
  }
}

remake_options <- function() {
  make_option <- optparse::make_option
  option_list <- list(
    make_option(c("-f", "--file"), type="character", default="remake.yml",
                help="remake file to load"),
    ## TODO: Rename this to avoid confusion with quiet_target?
    ## Probably do that at the same time as making verbose an integer
    ## with values 0, 1, ...
    make_option(c("-q", "--quiet"), type="logical", default=FALSE,
                action="store_true", help="Run quietly"),
    make_option(c("-p", "--print-targets"), type="logical",
               default=FALSE, action="store_true",
               dest="print_targets",
               help="Print names of valid targets and exit"),
    make_option(c("-n", "--dry-run"), type="logical",
                default=FALSE, action="store_true", dest="dry_run",
                help="Dry run (don't actually run anything)"),
    ## Not sure how to have this dump to a file when given with an
    ## argument but dump to stdout without.  Going with stdout.
    make_option(c("-s", "--script"), type="logical", default=FALSE,
                action="store_true", dest="script",
                help="Print R script to standard output"),
    make_option(c("-v", "--version"), type="logical", default=FALSE,
                action="store_true", help="Version information"))
}

## TODO: This will ideally also extract the github sha key if
## installed with devtools?  Or see tree for more ideas.
print_version <- function() {
  v <- packageVersion("remake")
  message(sprintf("remake version %s", v))
}

print_targets <- function(m) {
  message(paste(remake_target_names(m, FALSE), collapse="\n"))
}

##' Install running script to a local directory.  This directory
##' should be on the \code{$PATH}.  Once this has been done, you can
##' run remake with `remake`.  See `remake --help` for more information.
##' @title Install running script.
##'
##' The installed script is just a wrapper to the function
##' \code{\link{main}}; this means that upgrades to remake do not
##' requir this to be rerun.  The installed script is extremely
##' simple.
##' @param dest Directory to install `remake` to.  Should be on your
##' path, though the current directory may be useful to.
##' @export
install_remake <- function(dest) {
  if (!file.exists(dest) || !is_directory(dest)) {
    stop("Destination must be an existing directory")
  }
  code <- c("#!/usr/bin/env Rscript", "library(methods)", "remake::main()")
  file <- file.path(dest, "remake")
  writeLines(code, file)
  Sys.chmod(file, "0755")
}

##' @title Make a single target
##' @param target_names Vector of names of targets to build, or
##' \code{NULL} to build the default target (if specified in the
##' remakefile).
##' @param remake_file Name of the remakefile (by default
##' \code{remake.yml}).  This is passed to \code{remake()}.
##' @param verbose Controls whether remake is verbose or not.  By
##' default it is (\code{TRUE}), which prints out the name of each
##' target as it is built/checked.  This argument is passed to
##' \code{\link{remake_verbose}}; valid options are \code{TRUE},
##' \code{FALSE} and also the result of calling \code{remake_verbose}.
##' @export
make <- function(target_names=NULL, remake_file="remake.yml",
                 verbose=TRUE) {
  remake_make(remake(remake_file, verbose), target_names)
}

make_dependencies <- function(target_name, remake_file="remake.yml") {
  remake_dependencies(remake(remake_file), target_name)
}

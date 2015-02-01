##' The command line script, to be run outside of the package.  Don't
##' run this from within R (well, you can't actually).  This uses
##' \code{commandArgs} to pass along arguments to \code{\link{maker}}.
##' @title Command line interface to maker
##' @export
##' @param args Arguments to pass to
main <- function(args=commandArgs(TRUE)) {
  if (interactive()) {
    stop("This is not meant to be used from an R session!")
  }
  loadNamespace("optparse")
  parser <- optparse::OptionParser(option_list=maker_options())
  args <- optparse::parse_args(parser, args, positional_arguments=TRUE)
  opts <- args$options
  if (opts$version) {
    print_version()
    return(invisible())
  }
  m <- maker(opts$file, verbose=!opts$quiet)
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
    writeLines(maker_script(m, targets))
  } else {
    m$make(targets, dry_run=opts$dry_run)
  }
}

maker_options <- function() {
  make_option <- optparse::make_option
  ## TODO: make help the default option.
  option_list <- list(
    make_option(c("-f", "--file"), type="character", default="maker.yml",
                help="maker file to load"),
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
  v <- packageVersion("maker")
  message(sprintf("maker version %s", v))
}

print_targets <- function(m) {
  message(paste(m$target_names(), collapse="\n"))
}

##' Install running script to a local directory.  This directory
##' should be on the \code{$PATH}.  Once this has been done, you can
##' run maker with `maker`.  See `maker --help` for more information.
##' @title Install running script.
##'
##' The installed script is just a wrapper to the function
##' \code{\link{main}}; this means that upgrades to maker do not
##' requir this to be rerun.  The installed script is extremely
##' simple.
##' @param dest Directory to install `maker` to.  Should be on your
##' path, though the current directory may be useful to.
##' @export
install_maker <- function(dest) {
  ## TODO: Could be stdout(), could be a file?  Too much logic?  Be
  ## careful with the chmod if doing that.
  if (!file.exists(dest) || !is_directory(dest)) {
    stop("Destination must be an existing directory")
  }
  code <- c("#!/usr/bin/env Rscript", "library(methods)", "maker::main()")
  file <- file.path(dest, "maker")
  writeLines(code, file)
  Sys.chmod(file, "0755")
}

##' These are convenience functions that creates a maker object and
##' runs \code{make} or \code{script} on it.  For \code{make}, if the
##' final target is an object, that will be invisibly returned.  These
##' exist only to avoid creating a maker object before doing something
##' with it; this is all these functions do internally!
##' @title Make a single target
##' @param target_names Vector of names of targets to build, or
##' \code{NULL} to build the default target (if specified in the
##' makerfile).
##' @param maker_file Name of the makerfile (by default
##' \code{maker.yml}).  This is passed to \code{maker()}.
##' @export
make <- function(target_names=NULL, maker_file="maker.yml") {
  maker(maker_file)$make(target_names)
}

##' @rdname make
##' @export
make_script <- function(target_names=NULL, maker_file="maker.yml") {
  maker_script(maker(maker_file), target_names)
}

##' Install all missing packages that are required to use a
##' makerfile.  This is exactly equivalent to running
##' \code{maker::maker(maker_file)$install_packages()}, but is less
##' typing if you are not going to reuse the maker object.
##'
##' No version comparison is done - see packrat for a more complete
##' package management solution.
##' @title Install packages required by makerfile
##' @param maker_file Name of the makerfile (by default
##' \code{maker.yml}).  This is passed to \code{maker()}.
##' @export
maker_install_packages <- function(maker_file="maker.yml") {
  maker(maker_file)$install_packages()
}

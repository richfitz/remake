##' The command line script, to be run outside of the package.  Don't
##' run this from within R (well, you can't actually).  This uses
##' \code{commandArgs} to pass along arguments to \code{\link{maker}}.
##' @title Command line interface to maker
##' @export
##' @param args Arguments to pass to
##' @import optparse
main <- function(args=commandArgs(TRUE)) {
  if (interactive()) {
    stop("This is not meant to be used from an R session!")
  }
  args <- parse_args(OptionParser(option_list=maker_options()), args,
                     positional_arguments=TRUE)
  opts <- args$options
  if (opts$version) {
    print_version()
    return(invisible())
  }
  m <- maker$new(opts$file, verbose=!opts$quiet)
  if (opts$print_targets) {
    print_targets(m)
    return(invisible())
  }
  targets <- args$args
  if (length(targets) == 0L) {
    pos <- m$target_names()
    if (length(pos) == 0L) {
      stop("No targets found")
    } else {
      targets <- m$target_default()
    }
  }
  for (t in targets) {
    m$make(t, opts$dry_run)
  }
}

maker_options <- function() {
  ## TODO: make help the default option.
  option_list <- list(
    make_option(c("-f", "--file"), type="character", default="maker.yml",
                help="maker file to load"),
    make_option(c("-q", "--quiet"), type="logical", default=FALSE,
                action="store_true", help="Run quietly"),
    make_option(c("-p", "--print-targets"), type="logical",
               default=FALSE, action="store_true",
               dest="print_targets",
               help="Print names of valid targets and exit"),
    make_option(c("-n", "--dry-run"), type="logical",
                default=FALSE, action="store_true", dest="dry_run",
                help="Dry run (don't actually run anything)"),
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
  code <- c("#!/usr/bin/env Rscript", "maker::main()")
  file <- file.path(dest, "maker")
  writeLines(code, file)
  Sys.chmod(file, "0755")
}

##' This is a convenience function that creates a maker object and
##' builds a single target.  If that target is an object it will be
##' invisibly returned.
##' @title Make a single target
##' @param target Name of a target to build
##' @param maker_file Name of the makerfile (by default
##' \code{maker.yml}).  This is passed to \code{maker$new()}.
##' @param path Path to build in (by default the current directory).
##' Probably safest to leave this as-is.
##' @export
make <- function(target, maker_file="maker.yml", path=".") {
  maker$new(maker_file, path)$make(target)
}

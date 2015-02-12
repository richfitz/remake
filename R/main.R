##' This function exists to support commandline use, as installed by
##' \code{\link{install_remake}}.  This little script accepts standard
##' unix-style command-line options to drive a subset of remake's
##' functionality.  Don't run this function from within R!  This uses
##' \code{commandArgs} to pass along arguments to various remake
##' functions.
##'
##' See \code{remake --help} for instructions.
##'
##' At the moment, supported featues are:
##' \itemize{
##'
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
##' @title Command line interface to remake
##' @name remake
##' @rdname remake
NULL

main <- function(args=commandArgs(TRUE)) {
  remake_main_run(remake_main_parse_args(args))
}

remake_main_options <- function() {
  make_option <- optparse::make_option
  option_list <- list(
    make_option(c("-f", "--file"), type="character", default="remake.yml",
                help="remake file to load", dest="remake_file"),
    ## TODO: Rename this to avoid confusion with quiet_target?
    ## Probably do that at the same time as making verbose an integer
    ## with values 0, 1, ...
    make_option(c("-q", "--quiet"), type="logical", default=FALSE,
                action="store_true", help="Run quietly"),
    make_option(c("-l", "--list-targets"), type="logical",
               default=FALSE, action="store_true",
               dest="list_targets",
               help="List names of valid targets and exit"),
    make_option(c("-s", "--script"), type="logical", default=FALSE,
                action="store_true", dest="script",
                help="Print R script to standard output"),
    make_option("--script-file", type="character", default=NULL,
                dest="script_file"),
    make_option(c("-v", "--version"), type="logical", default=FALSE,
                action="store_true", help="Version information"))
}

remake_main_parse_args <- function(args) {
  parser <- optparse::OptionParser(option_list=remake_main_options())
  ret <- optparse::parse_args(parser, args, positional_arguments=TRUE)
  ## These should actually be allowed after '--'
  err <- grepl("^--", ret$args)
  if (any(err)) {
    stop("Invalid targets: ", paste(dquote(ret$args), collapse=", "))
  }
  if (!is.null(ret$options$script_file)) {
    ret$options$script <- TRUE
  }
  ret
}

remake_main_run <- function(args) {
  opts <- args$options

  if (opts$version) {
    ## TODO: This will ideally also extract the github sha key if
    ## installed with devtools?  Or see tree for more ideas.
    v <- packageVersion("remake")
    message(sprintf("remake version %s", v))
    return(invisible())
  }
  if (opts$list_targets) {
    message(paste(list_targets(opts$remake_file), collapse="\n"))
    return(invisible())
  }

  targets <- args$args
  if (length(targets) == 0L) {
    targets <- NULL
  }

  if (opts$script) {
    str <- make_script(targets,
                       verbose=!opts$quiet,
                       remake_file=opts$remake_file,
                       filename=opts$script_file)
    if (is.null(opts$script_file)) {
      writeLines(str)
    }
  } else {
    make(targets, remake_file=opts$remake_file, verbose=!opts$quiet)
  }
}

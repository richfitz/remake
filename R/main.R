##' The command line script, to be run outside of the package.  Don't
##' run this from within R (well, you can't actually).  This uses
##' \code{commandArgs} to pass along arguments to various remake
##' functions.
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
    ## TODO: This will ideally also extract the github sha key if
    ## installed with devtools?  Or see tree for more ideas.
    v <- packageVersion("remake")
    message(sprintf("remake version %s", v))
    return(invisible())
  }
  if (opts$print_targets) {
    message(paste(list_targets(opts$file), collapse="\n"))
    return(invisible())
  }
  m <- remake(opts$file, verbose=!opts$quiet)
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
    make_option(c("-l", "--list-targets"), type="logical",
               default=FALSE, action="store_true",
               dest="list_targets",
               help="List names of valid targets and exit"),
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

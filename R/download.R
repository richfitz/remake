## Name will change when the knitr stuff gets refactored.
##
## This is going to get augmented with code from Scott.
download_from_remake_target <- function(target, store, quiet=NULL) {
  quiet <- with_default(quiet, target$quiet)
  quiet <- with_default(quiet, FALSE)
  ## TODO: Deal with file:// urls, unless curl already does this?
  ## -- see ?url: only _absolute_ paths can be given, and that's not
  ## going to fly on Windows because the form looks like:
  ## `file://d:/path/to/file`
  url <- target$download

  dest <- target$name
  no_error <- file.exists(target$name)

  res <- try(download_file(url, dest, quiet), silent=TRUE)

  if (inherits(res, "try-error")) {
    ## Should really try and capture the error message here, because
    ## otherwise it's not very clear _why_ things fail; that's the
    ## root cause of #57.
    err_msg <- attr(res, "condition")$message
    msg <- sprintf("Downloading %s failed (from %s) with message:\n%s",
                   dest, url, err_msg)
    if (no_error) {
      ## TODO: At the moment this will never trigger because we'll
      ## never try to download a file directly...
      warning(msg, "\n\t...proceeding with existing file",
              immediate.=TRUE, call.=FALSE)
    } else {
      stop(msg)
    }
  }

  invisible(target$name)
}

## NOTE: This may trigger warnings because the built-in
## download/libcurl support warns if the reported and true lengths
## differ.  Not sure if this is really a problem though.
##
## The progress bar that downloader::download creates is much nicer.
## It's also very hard to tune in/out.  But for big files it's
## probably really required.  Argh.  There are no good choices, only
## less bad ones.
##
## It's possible (probable) that a failed download will overwrite the
## destination file; this will download elsewhere and move into place.
##
## More trouble; as documented in a recent issue, this really requires
## that downloader is installed.  However, that might no longer really
## be required if libcurl support is enabled.  I'm going to guess that
## is enabled on all offical distributions (so, OS/X, Windows, perhaps
## Debian/Ubuntu installations, too).  So in that case we can ignore
## downloader *unless* capabilities("libcurl") is FALSE _and_ the URL
## is an https:// url.
download_file <- function(url, dest, quiet) {
  tmp <- tempfile()
  status <- download.file(utils::URLencode(url), tmp, mode="wb")
  if (status != 0) {
    stop("Download failed with code ", status)
  }
  ok <- file.rename(tmp, dest)
  if (!ok) {
    ## Linux
    ok <- file.copy(tmp, dest, overwrite=TRUE)
    file.remove(tmp)
  }
  ok
}

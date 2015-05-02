## Name will change when the knitr stuff gets refactored.
##
## This is going to get augmented with code from Scott.
download_from_remake_target <- function(target, store, quiet=NULL) {
  quiet <- with_default(quiet, target$quiet)
  quiet <- with_default(quiet, FALSE)
  ## TODO: Deal with file:// urls, unless curl already does this?
  url <- target$download

  dest <- target$name
  no_error <- file.exists(target$name)

  res <- try(download_file(url, dest, quiet), silent=TRUE)

  if (inherits(res, "try-error")) {
    msg <- sprintf("Downloading %s failed (from %s)", dest, url)
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

## OK, so we want a new function to download a file and abstract over
## loading different packages to make it all work.  But I'm also cool
## with just using httr and falling back on download.file perhaps?
## Who knows.  But the dependency needs to be sorted out at some point
## so that we can *rely* on these functions on new computers.  Adding
## to `install_missing_packages()` is probably the best bet.
##
## The progress bar that downloader::download creates is much nicer.
## It's also very hard to tune in/out.  But for big files it's
## probably really required.  Argh.  There are no good choices, only
## less bad ones.
##
## It's possible (probable) that a failed download will overwrite the
## destination file; this will download elsewhere and move into place.
download_file <- function(url, dest, quiet, ...) {
  tmp <- tempfile()
  status <- downloader::download(url, tmp, quiet=quiet)
  if (status != 0) {
    stop("Download failed with code ", status)
  }
  ok <- file.rename(tmp, dest)
  if (!ok) {
    # Linux
    ok <- file.copy(tmp, dest)
    file.remove(tmp)
  }
  ok
}

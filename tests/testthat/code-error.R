## Code used to create errors during the build.

## From code.R:
download_data_works <- download_data

## This simulates how download.file fails; it leaves an empty file in
## the destination.  That's not great for us because if you run maker
## a *second* time it goes "oh sweet, the file has changed, let's
## rebuild" and you get rubbish.  Better is to restore the previous
## state on error.
download_data <- function(dest) {
  writeLines(character(0), dest) # write an empty file
  if (!exists(".run_download_data_works")) {
    stop("There was an error downloading data!")
  } else {
    download_data_works(dest)
  }
}

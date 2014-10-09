## If you had actual data to download the body of this function would
## look something like:
##   download.file(url, dest)
## "dest" here is the filename that the file will be saved in.
download_data <- function(dest) {
  write.csv(mtcars, dest)
}

## A function that does some processing on the data.  This converts
## "miles per gallon" to "kilometres per litre".  The "filename"
## argument is the name of the data set that will be operated on.  It
## returns the modified data set (we don't write anything to file).
process_data <- function(filename) {
  x <- read.csv(filename)
  x$kmpl <- x$mpg * 1.609344 / 3.78541
  x
}

## A function to plot the data.  It's not very impressive.  Note that
## we don't open any devices.
##
## The argument "dat" is the processed data (which will be generated
## by "process_data").
myplot <- function(dat) {
  plot(kmpl ~ disp, dat)
}

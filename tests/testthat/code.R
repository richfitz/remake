## A test.  There are two ways this could go:
##
## 1. Encode the filename yourself:
## download_data <- function() {
##   write.csv(mtcars, "data.csv")
## }
## 2. Encode the filename in the argument:
download_data <- function(dest) {
  write.csv(mtcars, dest)
}

process_data <- function(filename) {
  x <- read.csv(filename)
  x$kmpl <- x$mpg * 1.6 / 3.9
  x
}

do_plot <- function(dat, filename) {
  pdf(filename)
  on.exit(dev.off())
  plot(kmpl ~ disp, dat)
}

clean_hook <- function() {
  message("running post-cleanup hook")
}

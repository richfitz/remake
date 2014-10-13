download_data <- function(dest) {
  write.csv(mtcars, dest)
}

processed <- function(filename) {
  x <- read.csv(filename)
  x$kmpl <- x$mpg * 1.6 / 3.9
  x
}

do_plot <- function(dat, filename) {
  pdf(filename)
  on.exit(dev.off())
  myplot(dat)
}

myplot <- function(dat) {
  plot(kmpl ~ disp, dat)
}

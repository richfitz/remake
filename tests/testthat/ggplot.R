myggplot <- function(dat) {
  ggplot2::qplot(dat$disp, dat$kmpl)
}

do_myggplot <- function(dat, filename) {
  png(filename)
  on.exit(dev.off())
  print(myggplot(dat))
}

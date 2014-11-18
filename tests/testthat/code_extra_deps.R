## This version hardcodes the filename in.  It's not ideal practice,
## but might happen if the name is generated somehow, or if these
## files aren't really under the control of maker.
process_data_hardcoded <- function() {
  x <- read.csv("data.csv")
  x$kmpl <- x$mpg * 1.6 / 3.9
  x
}

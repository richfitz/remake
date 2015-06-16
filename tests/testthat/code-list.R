make_list <- function() {
  setNames(lapply(1:5, rnorm), letters[1:5])
}

square <- function(x) {
  x * x
}

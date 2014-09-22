bottom <- function(x) {
  mean(x)
}

single <- function(x) {
  x + bottom(x)
}

double <- function(x) {
  bottom(x) + single(x)
}

self <- function(x) {
  if (!is.null(x)) {
    self(NULL)
  } else {
    x
  }
}

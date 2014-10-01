noisy_message <- function() {
  message("make some noise")
  1
}

noisy_message2 <- function(x) {
  message("make some more noise")
  1
}

noisy_cat <- function() {
  cat("make some noise\n")
  1
}

noisy_warning <- function() {
  warning("make some noise")
  1
}

noisy_error <- function() {
  stop("make some noise")
  1
}

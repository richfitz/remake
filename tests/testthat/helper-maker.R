cleanup <- function() {
  unlink(".maker", recursive=TRUE)
  suppressWarnings(file.remove(c("data.csv", "plot.pdf")))
  invisible(NULL)
}

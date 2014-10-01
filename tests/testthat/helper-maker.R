cleanup <- function() {
  unlink(".maker", recursive=TRUE)
  suppressWarnings(file.remove(c("data.csv", "plot.pdf", "code2.R")))
  invisible(NULL)
}

cleanup <- function() {
  unlink(".maker", recursive=TRUE)
  suppressWarnings(file.remove(c("data.csv", "plot.pdf",
                                 "code2.R", "maker_error.yml",
                                 "plot2.pdf", "plot3.pdf", "plot4.pdf")))
  invisible(NULL)
}

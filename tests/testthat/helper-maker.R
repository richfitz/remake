cleanup <- function() {
  unlink(".maker", recursive=TRUE)
  suppressWarnings(file.remove(c("data.csv", "plot.pdf",
                                 "code2.R", "maker_error.yml",
                                 "plot1.pdf", "plot2.pdf",
                                 "plot3.pdf", "plot4.pdf",
                                 "knitr.md", "knitr_rename.md",
                                 "knitr_file_dep.md",
                                 "test.zip", "maker.zip",
                                 "tmp_quoting.yml")))
  unlink("figure", recursive=TRUE)
  unlink("test", recursive=TRUE)
  invisible(NULL)
}

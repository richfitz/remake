cleanup <- function() {
  unlink(".maker", recursive=TRUE)
  suppressWarnings(file.remove(c("data.csv", "plot.pdf",
                                 "code2.R", "maker_error.yml",
                                 "plot1.pdf", "plot2.pdf",
                                 "plot3.pdf", "plot4.pdf",
                                 "knitr.md", "knitr_rename.md",
                                 "knitr.tex",
                                 "knitr_file_dep.md",
                                 "test.zip", "maker.zip",
                                 "code_literal.R",
                                 "tmp_quoting.yml")))
  unlink("figure", recursive=TRUE)
  unlink("test", recursive=TRUE)
  invisible(NULL)
}

skip_unless_travis <- function() {
  if (identical(Sys.getenv("TRAVIS"), "true")) {
    return()
  }
  skip("Not on Travis")
}

if (is.null(getOption("repos"))) {
  options(repos="http://cran.rstudio.com")
}

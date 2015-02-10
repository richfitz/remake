cleanup <- function() {
  unlink(".remake", recursive=TRUE)
  suppressWarnings(file.remove(c("data.csv", "plot.pdf",
                                 "code2.R", "remake_error.yml",
                                 "plot1.pdf", "plot2.pdf",
                                 "plot3.pdf", "plot4.pdf",
                                 "knitr.md", "knitr_rename.md",
                                 "knitr.tex",
                                 "knitr_file_dep.md",
                                 "test.zip", "remake.zip",
                                 "code_literal.R",
                                 "remake_active.R",
                                 "tmp_quoting.yml")))
  unlink("figure", recursive=TRUE)
  unlink("test", recursive=TRUE)
  unlink("source_dir", recursive=TRUE)
  cache$clear()
  global_active_bindings$clear()
  invisible(NULL)
}

set_cran_mirror <- function() {
  if (is.null(getOption("repos"))) {
    options(repos="http://cran.rstudio.com")
  }
}

skip_unless_set <- function(name) {
  if (identical(Sys.getenv(name), "true")) {
    return()
  }
  skip("Skipping install package tests")
}

skip_unless_travis <- function() {
  skip_unless_set("TRAVIS")
}

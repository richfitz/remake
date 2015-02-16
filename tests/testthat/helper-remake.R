## Very cryptic way of setting CRAN mirrors apparently required,
## presumably to make R CMD check even more fun:
## http://stackoverflow.com/questions/16826933/installation-of-r-fpc-package
local({r <- getOption("repos");
       r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)})

cleanup <- function() {
  file_remove(".remake", recursive=TRUE)
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
                                 "remake",
                                 "tmp_quoting.yml")))
  file_remove("figure", recursive=TRUE)
  file_remove("test", recursive=TRUE)
  file_remove("source_dir", recursive=TRUE)
  remake:::cache$clear()
  remake:::global_active_bindings$clear()
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

with_options <- function(new, code) {
  old <- options(new)
  on.exit(options(old))
  force(code)
}

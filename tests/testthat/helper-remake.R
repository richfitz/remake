fake_empty_file <- function(filename) {
  writeLines(character(0), filename)
}

is_case_insensitive <- function() {
  Sys.info()[["sysname"]] %in% c("Darwin", "Windows")
}

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
                                 "remake_archive.zip",
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

## This is useful for debugging package installation problems that
## still can't be tested on Travis.  The docker approach shows the
## same errors so serves as an easier testbed.  Add this around calls
## to libPaths.
print_libpaths <- function(msg) {
  msg_libpaths <- paste0(".libPaths():\n",
                         paste("\t - ", .libPaths(), collapse="\n"))
  msg_packages1 <- paste0(".packages(): ", paste(.packages(), collapse=", "))
  msg_packages2 <- paste0(".packages(TRUE): ",
                          paste(.packages(TRUE), collapse=", "))
  msg <- "PACKAGE INSTALLATION START"
  msg <- paste(c(msg, msg_libpaths, msg_packages1, msg_packages2),
               collapse="\n")
  message(msg)
}

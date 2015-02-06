if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-remake.R")
}

context("Build")

test_that("Build works", {
  cleanup()
  m <- remake("remake.yml")

  m$make("plot.pdf", dry_run=TRUE)
  m$make("plot.pdf", dry_run=FALSE)
  m$make("plot.pdf", dry_run=FALSE)
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("Cleanup works", {
  cleanup()
  m <- remake("remake.yml")
  m$make("plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())

  m$make("clean")
  ## Checks that clean runs a hook:
  expect_that(m$make("clean"), shows_message("running post-cleanup hook"))
  expect_that(file.exists("data.csv"), is_true())
  expect_that(file.exists("plot.pdf"), is_false())

  ## Tidy won't run the hook:
  expect_that(m$make("tidy"), not(shows_message("running post-cleanup hook")))
  ## Purge will run the hook because it depends on clean
  expect_that(m$make("purge"), shows_message("running post-cleanup hook"))
  expect_that(file.exists("data.csv"), is_false())
})

test_that("Fake targets", {
  cleanup()
  m <- remake("remake.yml")
  expect_that(is_current("data.csv",  m), is_false())
  expect_that(is_current("processed", m), is_false())
  expect_that(is_current("plot.pdf",  m), is_false())
  m$make("all")
  expect_that(is_current("data.csv",  m), is_true())
  expect_that(is_current("processed", m), is_true())
  expect_that(is_current("plot.pdf",  m), is_true())
  cleanup()
})

test_that("Depending on a file we don't make", {
  ## Manually run the download step from before -- now we have a file
  ## that remake wants to depend on, but does not generate:
  e <- new.env()
  source("code.R", e)
  e$download_data("data.csv")
  expect_that(file.exists("data.csv"), is_true())
  m <- remake("remake2.yml")
  expect_that(file.exists("plot.pdf"), is_false())
  expect_that(m$make("plot.pdf"), not(throws_error()))
  expect_that(file.exists("plot.pdf"), is_true())
  m$make("purge")
  expect_that(file.exists("data.csv"), is_true())
  expect_that(file.exists("plot.pdf"), is_false())
  cleanup()
})

test_that("Error in source file", {
  cleanup()
  writeLines(c(readLines("code.R"), "}"), "code2.R")
  m <- remake()
  ## Ugly, and might not work in future:
  m$store$env$sources <- "code2.R"
  expect_that(m$store$env$reload(TRUE),
              throws_error("while sourcing 'code2.R'"))
  cleanup()
})

test_that("Error in yaml", {
  cleanup()
  expect_that(m <- remake("nonexistant.yml"),
              throws_error("'nonexistant.yml' does not exist"))
  writeLines(sub("command: download_data", " command: download_data",
                 readLines("remake.yml")),
             "remake_error.yml")

  expect_that(remake("remake_error.yml"),
              throws_error("while reading 'remake_error.yml'"))
  cleanup()
})

test_that("simple interface", {
  cleanup()
  make()
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("make_dependencies", {
  cleanup()
  m <- remake("remake.yml")
  e <- make_dependencies(m, "plot.pdf")
  expect_that(e, is_a("remake_environment"))

  expect_that(ls(e), equals("processed"))

  expect_that(remake_attach(e),
              shows_message("Remake environment for building"))
  expect_that("remake:plot.pdf" %in% search(), is_true())
  expect_that("remake:functions" %in% search(), is_true())

  expect_that(exists("processed"), is_true())
  expect_that(processed, is_a("data.frame"))

  expect_that(remake_detach(),
              shows_message("Detaching"))
  expect_that(remake_detach(),
              gives_warning("No remake environments found on search path"))
  expect_that(remake_detach(warn=FALSE),
              not(gives_warning()))
  expect_that(exists("processed"), is_false())

  expect_that(remake_attach(e, verbose=FALSE),
              not(shows_message()))
  expect_that(exists("processed"), is_true())
  expect_that(remake_detach(verbose=FALSE),
              not(shows_message()))
  expect_that(exists("processed"), is_false())
})

test_that("remake environment", {
  cleanup()
  m <- remake("remake.yml")
  expect_that(remake_environment(m, "processed"),
              throws_error("not found in object store"))
  m$make("processed")
  e <- remake_environment(m, "processed")
  expect_that(attr(e, "target"), is_null())
  expect_that(ls(e), equals("processed"))
  expect_that(exists("download_data", e), is_true())
})

test_that("extra dependencies", {
  cleanup()
  m <- remake("remake_extra_deps.yml")
  m$make("processed")
  expect_that(file.exists("data.csv"), is_true())
})

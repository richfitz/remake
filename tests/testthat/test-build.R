if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Build")

test_that("Build works", {
  cleanup()
  m <- maker("maker.yml")

  m$make("plot.pdf", dry_run=TRUE)
  m$make("plot.pdf", dry_run=FALSE)
  m$make("plot.pdf", dry_run=FALSE)
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("Cleanup works", {
  cleanup()
  m <- maker("maker.yml")
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
  m <- maker("maker.yml")
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
  ## that maker wants to depend on, but does not generate:
  e <- new.env()
  source("code.R", e)
  e$download_data("data.csv")
  expect_that(file.exists("data.csv"), is_true())
  m <- maker("maker2.yml")
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
  m <- maker()
  ## Ugly, and might not work in future:
  m$store$env$sources <- "code2.R"
  expect_that(m$store$env$reload(TRUE),
              throws_error("while sourcing 'code2.R'"))
  cleanup()
})

test_that("Error in yaml", {
  cleanup()
  expect_that(m <- maker("nonexistant.yml"),
              throws_error("'nonexistant.yml' does not exist"))
  writeLines(sub("command: download_data", " command: download_data",
                 readLines("maker.yml")),
             "maker_error.yml")

  expect_that(maker("maker_error.yml"),
              throws_error("while reading 'maker_error.yml'"))
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
  m <- maker("maker.yml")
  e <- make_dependencies(m, "plot.pdf")
  expect_that(e, is_a("maker_environment"))

  expect_that(ls(e), equals("processed"))

  expect_that(maker_attach(e),
              shows_message("Maker environment for building"))
  expect_that("maker:plot.pdf" %in% search(), is_true())
  expect_that("maker:functions" %in% search(), is_true())

  expect_that(exists("processed"), is_true())
  expect_that(processed, is_a("data.frame"))

  expect_that(maker_detach(),
              shows_message("Detaching"))
  expect_that(maker_detach(),
              gives_warning("No maker environments found on search path"))
  expect_that(maker_detach(warn=FALSE),
              not(gives_warning()))
  expect_that(exists("processed"), is_false())

  expect_that(maker_attach(e, verbose=FALSE),
              not(shows_message()))
  expect_that(exists("processed"), is_true())
  expect_that(maker_detach(verbose=FALSE),
              not(shows_message()))
  expect_that(exists("processed"), is_false())
})

test_that("maker environment", {
  cleanup()
  m <- maker("maker.yml")
  expect_that(maker_environment(m, "processed"),
              throws_error("not found in object store"))
  m$make("processed")
  e <- maker_environment(m, "processed")
  expect_that(attr(e, "target"), is_null())
  expect_that(ls(e), equals("processed"))
  expect_that(exists("download_data", e), is_true())
})

test_that("extra dependencies", {
  cleanup()
  m <- maker("maker_extra_deps.yml")
  m$make("processed")
  expect_that(file.exists("data.csv"), is_true())
})

if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Build")

test_that("Build works", {
  cleanup()
  m <- maker$new("maker.yml")
  m$make("plot.pdf", dry_run=TRUE)
  m$make("plot.pdf", dry_run=FALSE)
  m$make("plot.pdf", dry_run=FALSE)
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("Cleanup works", {
  cleanup()
  m <- maker$new("maker.yml")
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
  m <- maker$new("maker.yml")
  expect_that(m$is_current("data.csv"), is_false())
  expect_that(m$is_current("processed"), is_false())
  expect_that(m$is_current("plot.pdf"), is_false())
  m$make("all")
  expect_that(m$is_current("data.csv"),  is_true())
  expect_that(m$is_current("processed"), is_true())
  expect_that(m$is_current("plot.pdf"),  is_true())
  cleanup()
})

test_that("Depending on a file we don't make", {
  ## Manually run the download step from before -- now we have a file
  ## that maker wants to depend on, but does not generate:
  e <- new.env()
  source("code.R", e)
  e$download_data("data.csv")
  expect_that(file.exists("data.csv"), is_true())
  m <- maker$new("maker2.yml")
  expect_that(file.exists("plot.pdf"), is_false())
  expect_that(m$make("plot.pdf"), not(throws_error()))
  expect_that(file.exists("plot.pdf"), is_true())
  m$make("purge")
  expect_that(file.exists("data.csv"), is_true())
  expect_that(file.exists("plot.pdf"), is_false())
  cleanup()
})

test_that("Expiring targets", {
  devtools::load_all("../../")
  cleanup()
  m <- maker$new()
  m$make("plot.pdf")

  ## Sanity check:
  expect_that(file.exists("data.csv"), is_true())
  expect_that(m$is_current("data.csv"), is_true())
  expect_that(file.exists("plot.pdf"), is_true())
  expect_that(m$is_current("plot.pdf"), is_true())

  m$expire("plot.pdf")
  ## Still there:
  expect_that(file.exists("plot.pdf"), is_true())
  ## But not current:
  expect_that(m$is_current("plot.pdf"), is_false())

  ## Recursively expire:
  m$expire("plot.pdf", recursive=TRUE)
  expect_that(file.exists("data.csv"), is_true())
  expect_that(m$is_current("data.csv"), is_false())

  ## There's no easy way of catching this, but this has now rebuild
  ## everything, despite the files still being there.
  m$make()

  ## Works for objects as well as files:
  m$expire("processed")
  expect_that(m$is_current("processed"), is_false())
  expect_that(m$get("processed"), is_a("data.frame"))
  ## Because the *actual* R object is still there, the plot.pdf is
  ## considered current, but it would be rebuild because processed
  ## would be rebuilt.
  expect_that(m$is_current("plot.pdf"), is_true())
  m$make()

  cleanup()
})

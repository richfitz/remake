if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Build")

test_that("Build works", {
  cleanup()
  m <- maker$new("config.yml")
  m$make("plot.pdf", dry_run=TRUE)
  m$make("plot.pdf", dry_run=FALSE)
  m$make("plot.pdf", dry_run=FALSE)
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("Cleanup works", {
  cleanup()
  m <- maker$new("config.yml")
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
  m <- maker$new("config.yml")
  expect_that(m$is_current("data.csv"), is_false())
  expect_that(m$is_current("processed"), is_false())
  expect_that(m$is_current("plot.pdf"), is_false())
  m$make("all")
  expect_that(m$is_current("data.csv"),  is_true())
  expect_that(m$is_current("processed"), is_true())
  expect_that(m$is_current("plot.pdf"),  is_true())
  cleanup()
})

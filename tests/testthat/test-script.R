if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Script")

test_that("Build works", {
  cleanup()
  m <- maker$new("maker.yml")
  src <- m$script()
  expect_that(src, is_a("character"))
  dest <- tempfile()
  writeLines(src, dest)
  e <- new.env(parent=.GlobalEnv)
  source(dest, e)
  expect_that(ls(e), equals("processed"))
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("Build works with plotting target", {
  cleanup()
  m <- maker$new("maker3.yml")
  src <- m$script()
  expect_that(src, is_a("character"))
  e <- new.env(parent=.GlobalEnv)
  dest <- tempfile()
  writeLines(src, dest)
  source(dest, e)
  expect_that(ls(e), equals("processed"))
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

## I know that these need special care, so are disallowed at present:
test_that("Chained targets", {
  m <- maker$new("chain.yml")
  expect_that(m$script("chained"),
              throws_error("This needs some work still"))
})

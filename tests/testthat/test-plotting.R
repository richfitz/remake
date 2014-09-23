if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Plot")

test_that("Build works", {
  cleanup()
  m <- maker$new("maker3.yml")
  m$make("plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())
  ## TODO: ideally check that it is the expected size
  cleanup()
})

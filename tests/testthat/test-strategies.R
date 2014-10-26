if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Build strategies")

test_that("Plan", {
  m <- maker$new()
  expect_that(m$plan("plot.pdf"),
              equals(c("data.csv", "processed", "plot.pdf")))
  expect_that(m$plan("plot.pdf", dependencies_only=TRUE),
              equals(c("data.csv", "processed")))
})

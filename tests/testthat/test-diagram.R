if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Diagrams")

test_that("Can make diagram", {
  m <- maker$new("config.yml")
  expect_that(diagram(m), not(throws_error()))
  expect_that(m$diagram(), not(throws_error()))
})

if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Diagrams")

test_that("Can make diagram", {
  m <- maker("maker.yml")
  pdf("test-diagram-output.pdf")
  on.exit(dev.off()) # called after leaving this block, incl on error
  expect_that(diagram(m), not(throws_error()))
  expect_that(m$diagram(), not(throws_error()))
})

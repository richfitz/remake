if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Utilities")

test_that("insert_at", {
  y <- "value"
  expect_that(insert_at(list(), y, 1), equals(list(y)))
  expect_that(insert_at(character(0), y, 1), equals(y))

  expect_that(insert_at(list(), y, 0), throws_error("Invalid position"))
  expect_that(insert_at(list(), y, 2), throws_error("Invalid position"))
  expect_that(insert_at(list(), y, 1.1), throws_error("must be integer"))

  x <- list(a=1, b=2)
  expect_that(insert_at(x, y, 0), throws_error("Invalid position"))
  expect_that(insert_at(x, y, 1), equals(c(list("value"), x)))
  expect_that(insert_at(x, y, 2), equals(list(a=1, "value", b=2)))
  expect_that(insert_at(x, y, 3), equals(list(a=1, b=2, "value")))
  expect_that(insert_at(x, y, 4), throws_error("Invalid position"))

  x <- c(a="a", b="b")
  expect_that(insert_at(x, y, 0), throws_error("Invalid position"))
  expect_that(insert_at(x, y, 1), equals(c("value", x)))
  expect_that(insert_at(x, y, 2), equals(c(a="a", "value", b="b")))
  expect_that(insert_at(x, y, 3), equals(c(a="a", b="b", "value")))
  expect_that(insert_at(x, y, 4), throws_error("Invalid position"))
})

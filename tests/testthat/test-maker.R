if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Loading makerfiles")

test_that("Recursive makerfiles", {
  expect_that(maker$new("recursive1.yml"),
              throws_error("Recursive include detected"))
  expect_that(maker$new("recursive2.yml"),
              throws_error("Recursive include detected"))
  expect_that(maker$new("recursive3.yml"),
              throws_error("Recursive include detected"))
})

test_that("Target/rule clash", {
  cleanup()
  expect_that(m <- maker$new("maker_rule_clash.yml"),
              gives_warning("Rule name clashes with target name"))
  m$make()
  expect_that(file.exists("plot.pdf"), is_true())
})

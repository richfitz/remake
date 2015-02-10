if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-remake.R")
}

context("Modular remakefiles")

test_that("Modular remakefile", {
  cleanup()
  m <- remake("modular.yml")

  ## Not duplicated:
  expect_that(m$store$env$sources, equals("code.R"))
  expect_that("data.csv" %in% names(m$targets), is_true())
  ## data.csv is now listed *after* plot.pdf, because it was included
  ## afterwards.
  expect_that(names(m$targets)[1:4],
              equals(c("all", "processed", "plot.pdf", "data.csv")))
  expect_that(remake_default_target(m), equals("all"))

  mod <- remake("modular_module.yml")
  expect_that(remake_default_target(mod), equals("data.csv"))

  remake_make(m, "data.csv")

  expect_that(is_current("data.csv", m),   is_true())
  expect_that(is_current("data.csv", mod), is_true())

  remake_make(mod, "purge")
  expect_that(file.exists("data.csv"), is_false())
})

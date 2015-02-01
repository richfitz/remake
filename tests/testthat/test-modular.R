if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Modular makerfiles")

test_that("Modular makerfile", {
  cleanup()
  m <- maker("modular.yml")

  ## Not duplicated:
  expect_that(m$store$env$sources, equals("code.R"))
  expect_that("data.csv" %in% m$target_names(), is_true())
  ## data.csv is now listed *after* plot.pdf, because it was included
  ## afterwards.
  expect_that(m$target_names()[1:4],
              equals(c("all", "processed", "plot.pdf", "data.csv")))
  expect_that(maker_private(m)$target_default(), equals("all"))

  mod <- maker("modular_module.yml")
  mod$load_sources()
  expect_that(maker_private(mod)$target_default(), equals("data.csv"))

  m$make("data.csv")

  expect_that(m$is_current("data.csv"), is_true())
  expect_that(mod$is_current("data.csv"), is_true())

  mod$make("purge")
  expect_that(file.exists("data.csv"), is_false())
})

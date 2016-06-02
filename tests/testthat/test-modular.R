context("Modular remakefiles")

test_that("Modular remakefile", {
  cleanup()
  m <- remake("modular.yml")

  ## Not duplicated:
  expect_equal(m$store$env$sources, "code.R")
  expect_true("data.csv" %in% names(m$targets))
  ## data.csv is now listed *after* plot.pdf, because it was included
  ## afterwards.
  expect_equal(names(m$targets)[1:4],
               c("all", "processed", "plot.pdf", "data.csv"))
  expect_equal(remake_default_target(m), "all")

  mod <- remake("modular_module.yml")
  expect_equal(remake_default_target(mod), "data.csv")

  remake_make(m, "data.csv")

  expect_true(remake_is_current(m,   "data.csv"))
  expect_true(remake_is_current(mod, "data.csv"))

  remake_make(mod, "purge")
  expect_false(file.exists("data.csv"))
})

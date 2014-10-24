## These test various properties of targets.
if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Targets")

test_that("bootstrap", {
  expect_that(exists("install_github"), is_false())
  expect_that("devtools" %in% .packages(), is_false())
})

test_that("target with no extra packages", {
  cleanup()
  m <- maker$new("maker_target_packages.yml")
  t <- m$get_target("will_not_load")
  expect_that(t$packages, is_null())
  x <- m$make("will_not_load")
  expect_that(x, is_false())
})

test_that("target that loads extra package", {
  cleanup()
  m <- maker$new("maker_target_packages.yml")
  t <- m$get_target("will_load")
  expect_that(t$packages, equals("devtools"))
  x <- m$make("will_load")
  expect_that(x, is_true())
  expect_that("devtools" %in% .packages(), is_false())
})

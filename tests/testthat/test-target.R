## These test various properties of targets.
if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Targets")

test_that("Targets return their output on build", {
  cleanup()
  m <- maker$new("maker.yml")
  ## TODO: there's a really obscure error if this is not run but
  ## targets are.  Really need to catch that somewhere.
  m$load_sources()

  ## File targets will invisibly return their filename:
  t <- m$get_target("data.csv")
  expect_that(t$build(), equals("data.csv"))
  expect_that(m$build("data.csv"), equals("data.csv"))

  m$remove_target("data.csv")
  expect_that(m$update("data.csv"), equals("data.csv"))
  expect_that(m$update("data.csv"), equals("data.csv"))

  m$remove_target("data.csv")
  expect_that(m$make("data.csv"), equals("data.csv"))
  expect_that(m$make("data.csv"), equals("data.csv"))

  ## While object targets invisibly return their contents
  t <- m$get_target("processed")
  expect_that(t$build(), is_a("data.frame"))
  expect_that(m$build("processed"), is_a("data.frame"))

  m$remove_target("processed")
  expect_that(m$update("processed"), is_a("data.frame"))
  expect_that(m$update("processed"), is_a("data.frame"))

  m$remove_target("processed")
  expect_that(m$make("processed"), is_a("data.frame"))
  expect_that(m$make("processed"), is_a("data.frame"))

  ## Fake targets return nothing:
  t <- m$get_target("all")
  expect_that(t$build(), is_null())
  expect_that(m$build("all"), is_null())

  expect_that(m$remove_target("all"),
              throws_error("Not something that can be deleted"))
  expect_that(m$update("all"), is_null())
  expect_that(m$update("all"), is_null())

  expect_that(m$remove_target("all"),
              throws_error("Not something that can be deleted"))
  expect_that(m$make("all"), is_null())
  expect_that(m$make("all"), is_null())
})

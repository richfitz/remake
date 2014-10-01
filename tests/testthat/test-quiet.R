if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Quiet")

test_that("Quieten targets", {
  cleanup()
  m <- maker$new("quiet.yml")
  m$load_sources(FALSE)

  msg <- "make some noise"

  t <- m$get_target("noisy_message")
  expect_that(t$run(),            shows_message(msg))
  expect_that(t$run(quiet=TRUE),  not(shows_message()))
  expect_that(t$run(quiet=FALSE), shows_message(msg))

  t <- m$get_target("noisy_cat")
  expect_that(t$run(),            prints_text(msg))
  expect_that(t$run(quiet=TRUE),  not(shows_message()))
  expect_that(t$run(quiet=FALSE), prints_text(msg))

  t <- m$get_target("noisy_warning")
  expect_that(t$run(),            gives_warning(msg))
  expect_that(t$run(quiet=TRUE),  gives_warning(msg))
  expect_that(t$run(quiet=FALSE), gives_warning(msg))

  t <- m$get_target("noisy_error")
  expect_that(t$run(),            throws_error(msg))
  expect_that(t$run(quiet=TRUE),  throws_error(msg))
  expect_that(t$run(quiet=FALSE), throws_error(msg))
})

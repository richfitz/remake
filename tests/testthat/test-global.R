if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-remake.R")
}

context("Global mode")

test_that("Source changes trigger rebuild on variable access", {
  cleanup()
  filename_code <- "remake_active.R"
  code <- "foo <- function() 2"

  writeLines(code, filename_code)
  m <- remake::remake("remake_active.yml", envir=.GlobalEnv)
  expect_that(exists("obj", .GlobalEnv), is_true())
  expect_that(bindingIsActive("obj", .GlobalEnv), is_true())

  expect_that(x <- obj, shows_message("[ BUILD ] obj"))
  expect_that(x <- obj, not(shows_message()))

  code <- paste0(code, " * 2")
  writeLines(code, filename_code)
  expect_that(x <- obj, shows_message("loading sources"))
  expect_that(x <- obj, not(shows_message()))

  code <- paste0(code, " * 2")
  writeLines(code, filename_code)
  expect_that(x <- obj, shows_message("[ BUILD ] obj"))
  expect_that(x <- obj, not(shows_message()))
})

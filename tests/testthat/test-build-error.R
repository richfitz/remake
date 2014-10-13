if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

## First, arrange for the file to be made to exist so that deletion is
## a problem:

context("Build error")

test_that("Check mocking works", {
  cleanup()
  m <- maker$new("maker_build_error.yml")
  m$load_sources()

  ## Things are as they should be:
  m$store$env$env$download_data_works("data.csv")
  expect_that(file.exists("data.csv"), is_true())
  expect_that(file.info("data.csv")$size, is_more_than(0))

  ## No entry in the database:
  expect_that(m$store$db$contains("data.csv"),
              is_false())

  ## Manually run the broken case:
  expect_that(m$store$env$env$download_data("data.csv"),
              throws_error("There was an error downloading data!"))
  expect_that(file.exists("data.csv"), is_true())
  ## Oh noes! the file has been truncated!
  expect_that(file.info("data.csv")$size, equals(0))
  cleanup()
})

test_that("Errored builds restore files", {
  cleanup()
  m <- maker$new("maker_build_error.yml")
  m$load_sources()
  m$store$env$env$download_data_works("data.csv")
  hash <- tools::md5sum("data.csv")

  expect_that(m$make("data.csv"),
              throws_error("There was an error downloading data!"))
  expect_that(try(m$make("data.csv"), silent=TRUE),
              shows_message("Restoring previous version of data.csv"))
  ## Original unchanged:
  expect_that(tools::md5sum("data.csv"), equals(hash))

  ## Still no entry in the database:
  expect_that(m$store$db$contains("data.csv"),
              is_false())

  .GlobalEnv$.run_download_data_works <- TRUE
  m$make("data.csv")
  expect_that(file.exists("data.csv"), is_true())

  expect_that(m$store$db$contains("data.csv"), is_true())

  rm(.run_download_data_works, envir=.GlobalEnv)

  expect_that(m$make("data.csv", force=TRUE),
              throws_error("There was an error downloading data!"))
  expect_that(m$store$db$contains("data.csv"), is_true())
  expect_that(tools::md5sum("data.csv"), equals(hash))
})

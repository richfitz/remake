context("Build error")

test_that("Check mocking works", {
  ## First, arrange for the file to be made to exist so that deletion
  ## is a problem:
  cleanup()
  m <- remake("remake_build_error.yml")

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
  m <- remake("remake_build_error.yml")
  m$store$env$env$download_data_works("data.csv")
  hash <- tools::md5sum("data.csv")

  expect_that(remake_make(m, "data.csv"),
              throws_error("There was an error downloading data!"))
  expect_that(try(remake_make(m, "data.csv"), silent=TRUE),
              shows_message("Restoring previous version of data.csv"))
  ## Original unchanged:
  expect_that(tools::md5sum("data.csv"), equals(hash))

  ## Still no entry in the database:
  expect_that(m$store$db$contains("data.csv"),
              is_false())

  .GlobalEnv$.run_download_data_works <- TRUE
  remake_make(m, "data.csv")
  expect_that(file.exists("data.csv"), is_true())

  expect_that(m$store$db$contains("data.csv"), is_true())

  rm(.run_download_data_works, envir=.GlobalEnv)

  expect_that(remake_make(m, "data.csv", force=TRUE),
              throws_error("There was an error downloading data!"))
  expect_that(m$store$db$contains("data.csv"), is_true())
  expect_that(tools::md5sum("data.csv"), equals(hash))
})

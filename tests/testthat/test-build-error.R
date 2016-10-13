context("Build error")

test_that("Check mocking works", {
  ## First, arrange for the file to be made to exist so that deletion
  ## is a problem:
  cleanup()
  m <- remake("remake_build_error.yml")

  ## Things are as they should be:
  m$store$env$env$download_data_works("data.csv")
  expect_true(file.exists("data.csv"))
  expect_gt(file.info("data.csv")$size, 0)

  ## No entry in the database:
  expect_false(m$store$db$exists("data.csv"))

  ## Manually run the broken case:
  expect_error(m$store$env$env$download_data("data.csv"),
               "There was an error downloading data!")
  expect_true(file.exists("data.csv"))
  ## Oh noes! the file has been truncated!
  expect_equal(file.info("data.csv")$size, 0)
  cleanup()
})

test_that("Errored builds restore files", {
  cleanup()
  m <- remake("remake_build_error.yml")
  m$store$env$env$download_data_works("data.csv")
  hash <- tools::md5sum("data.csv")

  expect_error(remake_make(m, "data.csv"),
               "There was an error downloading data!")
  expect_message(try(remake_make(m, "data.csv"), silent=TRUE),
                 "Restoring previous version of data.csv")
  ## Original unchanged:
  expect_equal(tools::md5sum("data.csv"), hash)

  ## Still no entry in the database:
  expect_false(m$store$db$exists("data.csv"))

  .GlobalEnv$.run_download_data_works <- TRUE
  on.exit(rm(.run_download_data_works, envir=.GlobalEnv))

  remake_make(m, "data.csv")
  expect_true(file.exists("data.csv"))

  expect_true(m$store$db$exists("data.csv"))
})

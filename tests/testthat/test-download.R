context("Download targets")

test_that("download", {
  cleanup()
  obj <- remake(remake_file="download.yml")
  expect_is(obj$targets[["mtcars.R"]], "target_download")
  skip_unless_internet()

  ## TODO: Squash the output here.
  remake_make(obj, "mtcars.R")
  expect_true(file.exists("mtcars.R"))
  expect_message(remake_make(obj, "mtcars.R"), "OK")
  remake_make(obj, "clean")
  expect_true(file.exists("mtcars.R"))
  remake_make(obj, "purge")
  expect_false(file.exists("mtcars.R"))

  obj$targets[["mtcars.R"]]$quiet <- TRUE
  remake_make(obj)
  expect_true(file.exists("plot.pdf"))

  url <- obj$targets[["mtcars.R"]]$download
  obj$targets[["mtcars.R"]]$download <- "https://notarealhost/file"
  expect_warning(download_from_remake_target(obj$targets[["mtcars.R"]],
                                          obj$store),
                 "proceeding with existing file")

  remake_remove_target(obj, "mtcars.R")
  expect_error(remake_make(obj),
               "Downloading mtcars.R failed")
})

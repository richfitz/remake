context("Download targets")

test_that("download", {
  cleanup()
  obj <- remake(remake_file="download.yml")
  expect_that(obj$targets[["mtcars.R"]], is_a("target_download"))
  skip_unless_internet()

  ## TODO: Squash the output here.
  remake_make(obj, "mtcars.R")
  expect_that(file.exists("mtcars.R"), is_true())
  expect_that(remake_make(obj, "mtcars.R"), shows_message("OK"))
  remake_make(obj, "clean")
  expect_that(file.exists("mtcars.R"), is_true())
  remake_make(obj, "purge")
  expect_that(file.exists("mtcars.R"), is_false())

  obj$targets[["mtcars.R"]]$quiet <- TRUE
  remake_make(obj)
  expect_that(file.exists("plot.pdf"), is_true())

  url <- obj$targets[["mtcars.R"]]$download
  obj$targets[["mtcars.R"]]$download <- "https://notarealhost/file"
  expect_that(download_from_remake_target(obj$targets[["mtcars.R"]],
                                          obj$store),
              gives_warning("proceeding with existing file"))

  remake_remove_target(obj, "mtcars.R")
  expect_that(remake_make(obj),
              throws_error("Downloading mtcars.R failed"))
})

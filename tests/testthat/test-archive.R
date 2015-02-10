if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-remake.R")
}

context("Archive")

## Will need testing with files that are directories deep

## Archiving rules:
##   1. Probably rebuild first?  At least check current?
##   2. Take everything?  Take certain targets?
##   3. Could flag objects in/out?
##
## Alternatively getting the "current" state of the db might be
## worthwhile?  There will certainly be things to exclude.
test_that("Build archive", {
  cleanup()
  m <- remake("remake.yml")

  expect_that(remake_archive_export(m, "plot.pdf"),
              throws_error("file data.csv not found in file store"))
  remake_make(m, "processed")
  expect_that(remake_archive_export(m, "plot.pdf"),
              throws_error("file plot.pdf not found in file store"))
  remake_make(m, "plot.pdf")

  dest <- remake_archive_export(m, "plot.pdf")
  expect_that(dest, equals("remake.zip"))
  expect_that(file.exists("remake.zip"), is_true())

  contents <- unzip("remake.zip", list=TRUE)
  expect_that(all(c("remake/files/data.csv",
                    "remake/files/data.csv",
                    "remake/objects/processed") %in% contents$Name),
              is_true())
  cleanup()
})

test_that("Inspect archive", {
  cleanup()
  expect_that(is_remake_archive("remake.zip"),
              throws_error("The file 'remake.zip' does not exist"))

  m <- remake("remake.yml")
  remake_make(m)
  dest <- remake_archive_export(m, "plot.pdf")

  expect_that(is_remake_archive("remake.zip"),
              is_true())

  ## Could try here and get some fake zip archives tested, but
  ## crafting those by hand is a pain:
  tmp <- tempfile()
  tmp_path <- file.path(tmp, "remake")
  dir.create(tmp, recursive=TRUE)

  ## Missing one of the three top level directories:
  unzip("remake.zip", exdir=tmp)
  unlink(file.path(tmp_path, "db"), recursive=TRUE)
  zip_dir(tmp_path)
  expect_that(is_remake_archive("remake.zip"),
              is_false())
  expect_that(is_remake_archive("remake.zip", error=TRUE),
              throws_error("expected directories"))
  unlink(tmp_path, recursive=TRUE)

  ## Basically empty
  dir.create(tmp_path)
  zip_dir(tmp_path)
  expect_that(is_remake_archive("remake.zip"),
              is_false())
  expect_that(is_remake_archive("remake.zip", error=TRUE),
              throws_error("expected directories"))
  unlink(tmp_path, recursive=TRUE)
  cleanup()
})

test_that("Import archive", {
  cleanup()
  m <- remake("remake.yml")
  remake_make(m)
  dest <- remake_archive_export(m, "plot.pdf")
  remake_make(m, "purge")

  expect_that(is_current("data.csv", m),  is_false())
  expect_that(is_current("processed", m), is_false())
  expect_that(is_current("plot.pdf", m),  is_false())

  remake_archive_import(m, dest)

  expect_that(is_current("data.csv", m),  is_true())
  expect_that(is_current("processed", m), is_true())
  expect_that(is_current("plot.pdf", m),  is_true())

  cleanup()
})

if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Archive")

## Will need testing with files that are directories deep

## Archiving rules:
##   1. Probably rebuild first?  At least check current?
##   2. Take everything?  Take certain targets?
##   3. Could flag objects in/out?
##
## Alternatively getting the "current" state of the db might be
## worthwhile?  There will certainly be things to exclude.  Let's do
## this the exact same way as expire.
test_that("Build archive", {
  cleanup()
  m <- maker$new("maker_command.yml")

  expect_that(m$archive("plot.pdf"),
              throws_error("file data.csv not found in file store"))
  m$make("processed")
  expect_that(m$archive("plot.pdf"),
              throws_error("file plot.pdf not found in file store"))
  m$make("plot.pdf")

  dest <- m$archive("plot.pdf")
  expect_that(dest, equals("maker.zip"))
  expect_that(file.exists("maker.zip"), is_true())

  contents <- unzip("maker.zip", list=TRUE)
  expect_that(all(c("maker/files/data.csv",
                    "maker/files/data.csv",
                    "maker/objects/processed") %in% contents$Name),
              is_true())
  cleanup()
})

test_that("Inspect archive", {
  cleanup()
  expect_that(is_maker_archive("maker.zip"),
              throws_error("The file 'maker.zip' does not exist"))

  m <- maker$new("maker_command.yml")
  m$make()
  dest <- m$archive("plot.pdf")

  expect_that(is_maker_archive("maker.zip"),
              is_true())

  ## Could try here and get some fake zip archives tested, but
  ## crafting those by hand is a pain:
  tmp <- tempfile()
  tmp_path <- file.path(tmp, "maker")
  dir.create(tmp, recursive=TRUE)

  ## Missing one of the three top level directories:
  unzip("maker.zip", exdir=tmp)
  unlink(file.path(tmp_path, "db"), recursive=TRUE)
  zip_dir(tmp_path)
  expect_that(is_maker_archive("maker.zip"),
              is_false())
  unlink(tmp_path, recursive=TRUE)

  ## Basically empty
  dir.create(tmp_path)
  zip_dir(tmp_path)
  expect_that(is_maker_archive("maker.zip"),
              is_false())
  unlink(tmp_path, recursive=TRUE)
  cleanup()
})

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

  expect_error(remake_archive_export(m, "plot.pdf"),
               "Target not current: data.csv, processed")
  expect_error(remake_archive_export(m, "plot.pdf", dependencies=FALSE),
               "Target not current: plot.pdf")

  ## Build things:
  remake_make(m)

  remake_archive_export(m, "plot.pdf")
  expect_true(file.exists("remake.zip"))

  contents <- unzip("remake.zip", list=TRUE)$Name

  cmp <- file.path("remake/objects/keys/objects", storr::encode64("processed"))
  expect_true(cmp %in% contents)
  expect_true(all(c("remake/files/data.csv",
                    "remake/files/data.csv") %in% contents))
  cleanup()
})

test_that("Inspect archive", {
  cleanup()
  expect_error(is_archive("remake.zip"),
               "The file 'remake.zip' does not exist")

  m <- remake("remake.yml")
  remake_make(m)
  remake_archive_export(m, "plot.pdf")

  expect_true(is_archive("remake.zip"))
  expect_error(assert_remake_archive("remake.zip"), NA)

  ## Could try here and get some fake zip archives tested, but
  ## crafting those by hand is a pain:
  tmp <- tempfile()
  tmp_path <- file.path(tmp, "remake")
  dir.create(tmp, recursive=TRUE)

  ## Missing the remake.rds file:
  unzip("remake.zip", exdir=tmp)
  file_remove(file.path(tmp_path, "remake.rds"))
  zip_dir(tmp_path)
  expect_false(is_archive("remake.zip"))
  expect_error(assert_remake_archive("remake.zip"), "not a remake archive")
  file_remove(tmp_path, recursive=TRUE)

  ## Basically empty
  dir.create(tmp_path)
  zip_dir(tmp_path)
  expect_false(is_archive("remake.zip"))
  expect_error(assert_remake_archive("remake.zip"), "not a remake archive")
  file_remove(tmp_path, recursive=TRUE)
  cleanup()
})

test_that("Import archive", {
  cleanup()
  m <- remake("remake.yml")
  remake_make(m)
  dest <- remake_archive_export(m, "plot.pdf")
  remake_make(m, "purge")

  expect_false(remake_is_current(m, "data.csv"))
  expect_false(remake_is_current(m, "processed"))
  expect_false(remake_is_current(m, "plot.pdf"))

  remake_archive_import(m, dest)

  ## Magic!:
  expect_true(remake_is_current(m, "data.csv"))
  expect_true(remake_is_current(m, "processed"))
  expect_true(remake_is_current(m, "plot.pdf"))

  cleanup()
})

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
              throws_error("Target not current: data.csv, processed"))
  expect_that(remake_archive_export(m, "plot.pdf", dependencies=FALSE),
              throws_error("Target not current: plot.pdf"))

  ## Build things:
  remake_make(m)

  remake_archive_export(m, "plot.pdf")
  expect_that(file.exists("remake.zip"), is_true())

  contents <- unzip("remake.zip", list=TRUE)$Name

  cmp <- file.path("remake/objects/keys/objects", storr::encode64("processed"))
  expect_that(cmp %in% contents, is_true())
  expect_that(all(c("remake/files/data.csv",
                    "remake/files/data.csv") %in% contents),
              is_true())
  cleanup()
})

test_that("Inspect archive", {
  cleanup()
  expect_that(is_archive("remake.zip"),
              throws_error("The file 'remake.zip' does not exist"))

  m <- remake("remake.yml")
  remake_make(m)
  remake_archive_export(m, "plot.pdf")

  expect_that(is_archive("remake.zip"), is_true())
  expect_that(assert_remake_archive("remake.zip"),
              not(throws_error()))

  ## Could try here and get some fake zip archives tested, but
  ## crafting those by hand is a pain:
  tmp <- tempfile()
  tmp_path <- file.path(tmp, "remake")
  dir.create(tmp, recursive=TRUE)

  ## Missing the remake.rds file:
  unzip("remake.zip", exdir=tmp)
  file_remove(file.path(tmp_path, "remake.rds"))
  zip_dir(tmp_path)
  expect_that(is_archive("remake.zip"),
              is_false())
  expect_that(assert_remake_archive("remake.zip"),
              throws_error("not a remake archive"))
  file_remove(tmp_path, recursive=TRUE)

  ## Basically empty
  dir.create(tmp_path)
  zip_dir(tmp_path)
  expect_that(is_archive("remake.zip"),
              is_false())
  expect_that(assert_remake_archive("remake.zip"),
              throws_error("not a remake archive"))
  file_remove(tmp_path, recursive=TRUE)
  cleanup()
})

test_that("Import archive", {
  cleanup()
  m <- remake("remake.yml")
  remake_make(m)
  dest <- remake_archive_export(m, "plot.pdf")
  remake_make(m, "purge")

  expect_that(remake_is_current(m, "data.csv"),  is_false())
  expect_that(remake_is_current(m, "processed"), is_false())
  expect_that(remake_is_current(m, "plot.pdf"),  is_false())

  remake_archive_import(m, dest)

  ## Magic!:
  expect_that(remake_is_current(m, "data.csv"),  is_true())
  expect_that(remake_is_current(m, "processed"), is_true())
  expect_that(remake_is_current(m, "plot.pdf"),  is_true())

  cleanup()
})

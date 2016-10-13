context("API: Archive")

test_that("Archive export", {
  cleanup()
  make()

  expect_message(dest <- archive_export("all"),
                 "ZIP")
  expect_equal(dest, "remake.zip")
  expect_true(file.exists("remake.zip"))

  expect_true(is_archive("remake.zip"))
  expect_true(is_archive())
  expect_equal(sort(list_archive()),
               sort(c("processed", "data.csv", "plot.pdf")))

  d <- list_archive(detail=TRUE)
  expect_is(d, "data.frame")
  expect_identical(rownames(d), list_archive())
  expect_equal(names(d), c("type", "time", "hash"))
  ## TODO: implement an unsorted compare.
  expect_equal(sort(d$type), sort(c("object", "file", "file")))
  expect_is(d$time, "POSIXct")
  expect_is(d$hash, "character")

  make("purge")
  expect_message(archive_import(), "UNZIP")

  msg <- capture_messages(make())
  expect_false(any(grepl("BUILD", msg)))
  expect_equal(sum(grepl("OK", msg)), 3)
})

test_that("fetch_archive", {
  cleanup()

  expect_error(fetch_archive("processed"),
               "The file 'remake.zip' does not exist")

  make()
  v <- archive_export()
  d <- fetch_archive("processed")
  expect_identical(d, fetch("processed"))

  d <- fetch_archive("data.csv", path_prefix="archive_export")
  expect_equal(d, "archive_export/data.csv")
  expect_true(is_directory("archive_export"))
  hash <- hash_files("data.csv", FALSE)
  expect_equal(hash_files(d, FALSE), hash)

  file_remove("archive_export", TRUE)
  file_remove("data.csv")
  d <- fetch_archive("data.csv")
  expect_equal(d, "data.csv")
  expect_true(file.exists("data.csv"))
  expect_equal(hash_files("data.csv", FALSE), hash)

  expect_error(fetch_archive("all"),
               "all not found in archive remake.zip")
})

## This checks that the archive_file command is correctly passed
## through:
test_that("Other filenames (archive)", {
  filename <- "remake_archive.zip"
  cleanup()
  make()
  expect_false(file.exists("remake.zip"))
  expect_false(file.exists(filename))

  archive_export(archive_file=filename)
  expect_false(file.exists("remake.zip"))
  expect_true(file.exists(filename))
  expect_true(is_archive(filename))
  expect_equal(sort(list_archive(filename)),
               sort(c("processed", "data.csv", "plot.pdf")))

  expect_identical(fetch_archive("processed", archive_file=filename),
                   fetch("processed"))

  make("clean")
  expect_error(archive_import(),
               "The file 'remake.zip' does not exist")
  archive_import(filename)

  msg <- capture_messages(make())
  expect_false(any(grepl("BUILD", msg)))
})

## And this that the remake_file argument is passed through
test_that("Other filenames (remake)", {
  cleanup()
  filename_remake <- "plot_simple.yml"
  filename_archive <- "remake_archive.zip"

  make(remake_file=filename_remake)
  expect_false(file.exists("remake.zip"))
  expect_false(file.exists(filename_archive))

  archive_export(archive_file=filename_archive,
                 remake_file=filename_remake)
  expect_false(file.exists("remake.zip"))
  expect_true(file.exists(filename_archive))
  expect_true(is_archive(filename_archive))
  expect_equal(sort(list_archive(filename_archive)),
              sort(c("processed", "data.csv", "plot.pdf")))

  expect_identical(fetch_archive("processed", archive_file=filename_archive),
              fetch("processed", remake_file=filename_remake))

  make("clean", remake_file=filename_remake)
  msg <- capture_messages(archive_import(filename_archive,
                                         remake_file=filename_remake))
  expect_false(any(grepl("LOAD", msg)))

  msg <- capture_messages(make(remake_file=filename_remake))
  expect_false(any(grepl("BUILD", msg)))
})

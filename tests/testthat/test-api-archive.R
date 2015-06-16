context("API: Archive")

test_that("Archive export", {
  cleanup()
  make()

  expect_that(dest <- archive_export("all"),
              shows_message("ZIP"))
  expect_that(dest, equals("remake.zip"))
  expect_that(file.exists("remake.zip"), is_true())

  expect_that(is_archive("remake.zip"), is_true())
  expect_that(is_archive(), is_true())
  expect_that(sort(list_archive()),
              equals(sort(c("processed", "data.csv", "plot.pdf"))))


  d <- list_archive(detail=TRUE)
  expect_that(d, is_a("data.frame"))
  expect_that(rownames(d), is_identical_to(list_archive()))
  expect_that(names(d), equals(c("type", "time", "hash")))
  ## TODO: implement an unsorted compare.
  expect_that(sort(d$type),
              equals(sort(c("object", "file", "file"))))
  expect_that(d$time, is_a("POSIXct"))
  expect_that(d$hash, is_a("character"))

  make("clean")
  expect_that(archive_import(),
              shows_message("UNZIP"))
  expect_that(d <- make(),
              not(shows_message("BUILD")))
})

test_that("fetch_archive", {
  cleanup()

  expect_that(fetch_archive("processed"),
              throws_error("The file 'remake.zip' does not exist"))

  make()
  v <- archive_export()
  d <- fetch_archive("processed")
  expect_that(d, is_identical_to(fetch("processed")))

  d <- fetch_archive("data.csv", path_prefix="archive_export")
  expect_that(d, equals("archive_export/data.csv"))
  expect_that(is_directory("archive_export"), is_true())
  hash <- hash_files("data.csv", FALSE)
  expect_that(hash_files(d, FALSE), equals(hash))

  file_remove("archive_export", TRUE)
  file_remove("data.csv")
  d <- fetch_archive("data.csv")
  expect_that(d, equals("data.csv"))
  expect_that(file.exists("data.csv"), is_true())
  expect_that(hash_files("data.csv", FALSE), equals(hash))

  expect_that(fetch_archive("all"),
              throws_error("all not found in archive remake.zip"))
})

## This checks that the archive_file command is correctly passed
## through:
test_that("Other filenames (archive)", {
  filename <- "remake_archive.zip"
  cleanup()
  make()
  expect_that(file.exists("remake.zip"), is_false())
  expect_that(file.exists(filename), is_false())

  archive_export(archive_file=filename)
  expect_that(file.exists("remake.zip"), is_false())
  expect_that(file.exists(filename), is_true())
  expect_that(is_archive(filename), is_true())
  expect_that(sort(list_archive(filename)),
              equals(sort(c("processed", "data.csv", "plot.pdf"))))

  expect_that(fetch_archive("processed", archive_file=filename),
              is_identical_to(fetch("processed")))

  make("clean")
  expect_that(archive_import(),
              throws_error("The file 'remake.zip' does not exist"))
  archive_import(filename)

  expect_that(make(), not(shows_message("BUILD")))
})

## And this that the remake_file argument is passed through
test_that("Other filenames (remake)", {
  cleanup()
  filename_remake <- "plot_simple.yml"
  filename_archive <- "remake_archive.zip"

  make(remake_file=filename_remake)
  expect_that(file.exists("remake.zip"), is_false())
  expect_that(file.exists(filename_archive), is_false())

  archive_export(archive_file=filename_archive,
                 remake_file=filename_remake)
  expect_that(file.exists("remake.zip"), is_false())
  expect_that(file.exists(filename_archive), is_true())
  expect_that(is_archive(filename_archive), is_true())
  expect_that(sort(list_archive(filename_archive)),
              equals(sort(c("processed", "data.csv", "plot.pdf"))))

  expect_that(fetch_archive("processed", archive_file=filename_archive),
              is_identical_to(fetch("processed", remake_file=filename_remake)))

  make("clean", remake_file=filename_remake)
  expect_that(archive_import(filename_archive,
                             remake_file=filename_remake),
              not(shows_message("LOAD")))

  expect_that(make(remake_file=filename_remake),
              not(shows_message("BUILD")))
})

context("API: Archive")

test_that("Archive export", {
  cleanup()
  make()

  dest <- archive_export("all")
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
  archive_import()
  expect_that(d <- make(),
              not(shows_message("BUILD")))
})

test_that("archive with chain targets", {
  cleanup()
  ## Archive with chained targets:
  make(c("manual", "chained"), remake_file="chain.yml")
  archive_export(c("manual", "chained"), remake_file="chain.yml")

  expected <- list_targets(remake_file="chain.yml",
                           type="object",
                           include_chain_intermediates=TRUE)
  expect_that(sort(list_archive()),
              equals(sort(expected)))

  ## TODO: This will be easier when we have current checks in the api.
  make("clean", remake_file="chain.yml")
  archive_import(remake_file="chain.yml")
  expect_that(d <- make("chained", remake_file="chain.yml"),
              not(shows_message("BUILD")))
})

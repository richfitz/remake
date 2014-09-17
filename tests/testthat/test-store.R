if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
}

context("Data stores")

test_that("data store", {
  path <- tempfile()
  st <- maker:::object_store$new(path)
  expect_that(file.exists(path), is_true())
  expect_that(st$ls(), equals(character(0)))
  expect_that(st$contains("foo"), is_false())

  ## Not clear if this is correct behaviour.  stale() is going to
  ## be more useful, and in that case we'd want TRUE, not error.
  ## expect_that(st$older_than("foo", Sys.time()),
  ##             throws_error())
  st$set("foo", 1:10)
  expect_that(st$contains("foo"), is_true())
  h <- st$get_hash("foo")
  expect_that(unname(h), equals(digest::digest(1:10)))
  expect_that(unname(h), not(equals(digest::digest(as.numeric(1:10)))))

  obj <- st$get("foo")
  expect_that(obj, is_identical_to(1:10))

  expect_that(st$get_hash("bar"), throws_error("not found in object store"))
  expect_that(st$get_hash("bar", TRUE), is_identical_to(NA_character_))
  expect_that(st$get_hash("foo", TRUE), equals(digest::digest(1:10)))

  ## A different data store would see this:
  st2 <- maker:::object_store$new(path)
  st2$contains("foo")

  st$del("foo")
  expect_that(st$contains("foo"), is_false())
  expect_that(st2$contains("foo"), is_false())

  unlink(path, recursive=TRUE)
})

test_that("file store", {
  st <- maker:::file_store$new()

  path <- tempdir()
  file <- file.path(path, "test.txt")
  writeLines(letters, file)
  expect_that(st$contains(file), is_true())

  h <- st$get_hash(file)
  expect_that(h, equals(unname(tools::md5sum(file))))

  expect_that(st$get_hash("not/in/store"),
              throws_error("not found in file store"))
  expect_that(st$get_hash("not/in/store", TRUE),
              is_identical_to(NA_character_))

  st$del(file)
  expect_that(st$contains(file), is_false())
})

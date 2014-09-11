if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
}

context("data store")

test_that("data store", {
  path <- tempfile()
  st <- store$new(path)
  expect_that(file.exists(file.path(path, ".maker")), is_true())
  expect_that(st$data$ls(), equals(character(0)))
  expect_that(st$data$contains("foo"), is_false())

  ## Not clear if this is correct behaviour.  stale() is going to
  ## be more useful, and in that case we'd want TRUE, not error.
  ## expect_that(st$data$older_than("foo", Sys.time()),
  ##             throws_error())
  st$data$store("foo", 1:10)
  expect_that(st$data$contains("foo"), is_true())
  h <- st$data$get_hash("foo")
  expect_that(unname(h), equals(digest::digest(1:10)))
  expect_that(st$data$has_hash("foo", digest::digest(1:10)),
              is_true())
  expect_that(st$data$has_hash("foo", digest::digest(as.numeric(1:10))),
              is_false())

  ## A different data store would see this:
  st2 <- store$new(path)
  st2$data$contains("foo")

  st$data$rm("foo")
  expect_that(st$data$contains("foo"), is_false())
  expect_that(st2$data$contains("foo"), is_false())

  st$data$destroy()
  expect_that(file.exists(file.path(path, ".maker")), is_false())
})

test_that("file store", {
  path <- tempfile()
  st <- store$new(path)

  file <- file.path(path, "test.txt")
  writeLines(letters, file)
  expect_that(st$files$contains(file), is_true())

  h <- st$files$get_hash(file)
  expect_that(st$files$has_hash(file, h), is_true())
  st$files$rm(file)
  expect_that(st$files$contains(file), is_false())
})

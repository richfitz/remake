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

  expect_that(st$get("bar"), throws_error("not found in object store"))
  expect_that(st$get_hash("bar"), throws_error("not found in object store"))

  expect_that(st$get_hash("bar", TRUE), is_identical_to(NA_character_))
  expect_that(st$get_hash("foo", TRUE), equals(digest::digest(1:10)))

  ## A different data store would see this:
  st2 <- maker:::object_store$new(path)
  st2$contains("foo")

  ## Can export things:
  e <- new.env(parent=emptyenv())
  st2$export("foo", e)
  expect_that(ls(e), equals("foo"))
  expect_that(e$foo, is_identical_to(1:10))

  ## Can export zero things:
  e <- new.env(parent=emptyenv())
  st2$export(character(0), e)
  expect_that(ls(e), equals(character(0)))

  ## Can export everything
  st$set("bar", runif(10))
  expect_that(st$ls(), equals(c("bar", "foo")))
  e <- new.env(parent=emptyenv())
  st$export(envir=e)
  expect_that(ls(e), equals(c("bar", "foo")))

  ## Can rename things on export:
  e <- new.env(parent=emptyenv())
  st$export(c(Bar="bar", Foo="foo"), e)
  expect_that(ls(e), equals(c("Bar", "Foo")))
  expect_that(e$Bar, is_identical_to(st$get("bar")))
  expect_that(e$Foo, is_identical_to(st$get("foo")))

  ## Pathalogical rename:
  e <- new.env(parent=emptyenv())
  st$export(c(foo="bar", bar="foo"), e)
  expect_that(ls(e), equals(c("bar", "foo")))
  expect_that(e$foo, is_identical_to(st$get("bar")))
  expect_that(e$bar, is_identical_to(st$get("foo")))

  st$del("foo")
  expect_that(st$contains("foo"), is_false())
  expect_that(st2$contains("foo"), is_false())

  expect_that(st$ls(), equals("bar"))

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

test_that("storing lists", {
  path <- tempfile()
  st <- maker:::object_store$new(path)

  x <- as.list(1:10)
  st$set("as_object", x)
  st$set("as_list",   x, as_list=TRUE)

  expect_that(st$get("as_list"), equals(st$get("as_object")))
})

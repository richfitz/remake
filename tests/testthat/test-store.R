context("Data stores")

test_that("file store", {
  st <- file_store$new()

  path <- tempdir()
  file <- file.path(path, "test.txt")
  writeLines(letters, file)
  expect_that(st$exists(file), is_true())

  h <- st$get_hash(file)
  expect_that(h, equals(unname(tools::md5sum(file))))

  expect_that(st$get_hash("not/in/store"),
              throws_error("not found in file store"))
  expect_that(st$exists("not/in/store"),
              is_false())

  st$del(file)
  expect_that(st$exists(file), is_false())
})

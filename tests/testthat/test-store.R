context("Data stores")

test_that("file store", {
  st <- file_store$new()

  path <- tempdir()
  file <- file.path(path, "test.txt")
  writeLines(letters, file)
  expect_true(st$exists(file))

  h <- st$get_hash(file)
  expect_equal(h, unname(tools::md5sum(file)))

  expect_error(st$get_hash("not/in/store"), "not found in file store")
  expect_false(st$exists("not/in/store"))

  st$del(file)
  expect_false(st$exists(file))
})

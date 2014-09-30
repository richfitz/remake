if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
}

context("List storage")

test_that("Store and get a list", {
  path <- tempfile()
  st <- list_store$new(path)
  expect_that(st$len(), equals(0))

  obj <- as.list(1:10)
  st$set_list(obj)

  expect_that(st$len(), equals(10))
  expect_that(st$get_list(), equals(obj))
  expect_that(st$complete(), is_true())

  expect_that(file.exists(path), is_true())
  expect_that(is_directory(path), is_true())

  ## Point a second list instance at that object and check that it
  ## agrees.
  st2 <- list_store$new(path)
  expect_that(st2$len(), equals(10))
  expect_that(st2$get_list(), equals(obj))
  expect_that(st2$complete(), is_true())

  ## Get, with bounds checking:
  expect_that(st$get(3), equals(obj[[3]]))
  expect_that(st$get(3, check_bounds=FALSE), equals(obj[[3]]))
  expect_that(st$get(11), throws_error("Bounds must be"))

  ## Set, with bounds checking:
  st$set(3, 30)
  expect_that(st$get(3), equals(30))
  expect_that(st2$get(3), equals(30))
  obj2 <- obj
  obj2[[3]] <- 30
  expect_that(st$get_list(), equals(obj2))
  expect_that(st2$get_list(), equals(obj2))

  st$set(4, 40, check_bounds=FALSE)
  expect_that(st$get(4), equals(40))

  expect_that(st$set(11, 110), throws_error("Bounds must be"))

  st$del_list()
  expect_that(file.exists(path), is_false())
  ## These should be tidied up?
  expect_that(suppressWarnings(st$len()),
              throws_error("cannot open the connection"))
  expect_that(suppressWarnings(st2$len()),
              throws_error("cannot open the connection"))
})

test_that("Empty list", {
  path <- tempfile()
  st <- list_store$new(path)
  st$set_list(list())
  expect_that(st$len(), equals(0))
  expect_that(st$get_list(), equals(list()))
})

test_that("Named list", {
  path <- tempfile()
  st <- list_store$new(path)
  obj <- as.list(1:10)
  names(obj) <- letters[seq_along(obj)]
  st$set_list(obj)
  expect_that(st$get_list(), equals(obj))
})

test_that("Attributes", {
  path <- tempfile()
  st <- list_store$new(path)
  obj <- as.list(1:10)
  dim(obj) <- c(2, 5)
  attr(obj, "custom_attribute") <- runif(10)
  st$set_list(obj)
  expect_that(st$get_list(), equals(obj))
})

test_that("Non list objects", {
  path <- tempfile()
  st <- list_store$new(path)
  obj <- 1:10
  expect_that(st$set_list(obj),
              gives_warning("nontransitive"))
  expect_that(st$get_list(), equals(as.list(obj)))
})

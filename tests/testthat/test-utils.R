context("Utilities")

test_that("insert_at", {
  y <- "value"
  expect_that(insert_at(list(), y, 1), equals(list(y)))
  expect_that(insert_at(character(0), y, 1), equals(y))

  expect_that(insert_at(list(), y, 0), throws_error("Invalid position"))
  expect_that(insert_at(list(), y, 2), throws_error("Invalid position"))
  expect_that(insert_at(list(), y, 1.1), throws_error("must be integer"))

  x <- list(a=1, b=2)
  expect_that(insert_at(x, y, 0), throws_error("Invalid position"))
  expect_that(insert_at(x, y, 1), equals(c(list("value"), x)))
  expect_that(insert_at(x, y, 2), equals(list(a=1, "value", b=2)))
  expect_that(insert_at(x, y, 3), equals(list(a=1, b=2, "value")))
  expect_that(insert_at(x, y, 4), throws_error("Invalid position"))

  x <- c(a="a", b="b")
  expect_that(insert_at(x, y, 0), throws_error("Invalid position"))
  expect_that(insert_at(x, y, 1), equals(c("value", x)))
  expect_that(insert_at(x, y, 2), equals(c(a="a", "value", b="b")))
  expect_that(insert_at(x, y, 3), equals(c(a="a", b="b", "value")))
  expect_that(insert_at(x, y, 4), throws_error("Invalid position"))
})

test_that("zip_dir", {
  dir.create("test")
  file.copy(c("code.R", "remake.yml"), "test")

  dest <- zip_dir("test")
  expect_that(dest, equals("test.zip"))
  expect_that(file.exists("test.zip"), is_true())

  contents <- unzip("test.zip", list=TRUE)
  expected <- c("test/code.R", "test/remake.yml")
  expect_that(all(expected %in% contents$Name), is_true())
  file.remove("test.zip")

  ## Then, out of place:
  path <- file.path(tempdir(), "test")
  dir.create(path)
  file.copy(c("code.R", "remake.yml"), path)

  dest <- zip_dir(path)
  expect_that(dest, equals("test.zip"))
  expect_that(all(expected %in% contents$Name), is_true())
  file.remove("test.zip")
})

test_that("git_exists", {
  ## Definitely not in a temp directory:
  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  expect_that(git_exists(), not(throws_error()))
  expect_that(git_exists(), not(gives_warning()))
  expect_that(git_exists(), is_false())

  expect_that(git_ignores(character(0)), equals(logical(0)))
  expect_that(git_ignores("foo"), equals(FALSE))
})

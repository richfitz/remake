context("Utilities")

test_that("insert_at", {
  y <- "value"
  expect_equal(insert_at(list(), y, 1), list(y))
  expect_equal(insert_at(character(0), y, 1), y)

  expect_error(insert_at(list(), y, 0), "Invalid position")
  expect_error(insert_at(list(), y, 2), "Invalid position")
  expect_error(insert_at(list(), y, 1.1), "must be integer")

  x <- list(a=1, b=2)
  expect_error(insert_at(x, y, 0), "Invalid position")
  expect_equal(insert_at(x, y, 1), c(list("value"), x))
  expect_equal(insert_at(x, y, 2), list(a=1, "value", b=2))
  expect_equal(insert_at(x, y, 3), list(a=1, b=2, "value"))
  expect_error(insert_at(x, y, 4), "Invalid position")

  x <- c(a="a", b="b")
  expect_error(insert_at(x, y, 0), "Invalid position")
  expect_equal(insert_at(x, y, 1), c("value", x))
  expect_equal(insert_at(x, y, 2), c(a="a", "value", b="b"))
  expect_equal(insert_at(x, y, 3), c(a="a", b="b", "value"))
  expect_error(insert_at(x, y, 4), "Invalid position")
})

test_that("zip_dir", {
  dir.create("test")
  file.copy(c("code.R", "remake.yml"), "test")

  dest <- zip_dir("test")
  expect_equal(dest, "test.zip")
  expect_true(file.exists("test.zip"))

  contents <- unzip("test.zip", list=TRUE)
  expected <- c("test/code.R", "test/remake.yml")
  expect_true(all(expected %in% contents$Name))
  file_remove("test.zip")

  ## Then, out of place:
  path <- file.path(tempdir(), "test")
  dir.create(path)
  file.copy(c("code.R", "remake.yml"), path)

  dest <- zip_dir(path)
  expect_equal(dest, "test.zip")
  expect_true(all(expected %in% contents$Name))
  file_remove("test.zip")
})

test_that("git_exists", {
  ## Definitely not in a temp directory:
  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  expect_error(git_exists(), NA)
  expect_warning(git_exists(), NA)
  expect_false(git_exists())

  expect_equal(git_ignores(character(0)), logical(0))
  expect_equal(git_ignores("foo"), FALSE)
})

test_that("file_exists", {
  expect_true(file.exists("test-target.R"))
  ## This will be true on Mac and Windows; Linux and unixes will not
  ## see the file.
  expect_equal(file.exists("test-target.r"), is_case_insensitive())

  expect_false(file_exists("test-target.r"))

  files <- dir(".", recursive=TRUE)
  ## There's a nicer way of doing this with sub I think.
  len <- nchar(files)
  files_lower <- paste0(substr(files, 1, len - 1),
                        tolower(substr(files, len, len)))
  files_upper <- paste0(substr(files, 1, len - 1),
                        toupper(substr(files, len, len)))
  ## Case munging sees them all:
  expect_equal(file.exists(files_lower),
               is_case_insensitive() | files == files_lower)
  expect_equal(file.exists(files_upper),
               is_case_insensitive() | files == files_upper)

  expect_equal(file_exists(files_lower), files == files_lower)
  expect_equal(file_exists(files_upper), files == files_upper)

  ## Mix in some nonexistant things:
  fake <- c("test/fake.r", "fakedir/fake.r", "fake.r")
  o <- sample(length(files) + length(fake))
  files2 <- c(files, fake)[o]
  files2_lower <- c(files_lower, fake)[o]
  files2_upper <- c(files_upper, fake)[o]

  exists <- !(files2 %in% fake)
  expect_equal(file.exists(files2), exists)
  expect_equal(file_exists(files2_lower), exists & files2 == files2_lower)
  expect_equal(file_exists(files2_upper), exists & files2 == files2_upper)

  expect_equal(file_real_case(files), files)
  if (is_case_insensitive()) {
    expect_equal(file_real_case(files_upper), files)
    expect_equal(file_real_case(files_lower), files)
  }

  files2_real <- ifelse(exists, files2, NA_character_)
  expect_equal(file_real_case(files2), files2_real)
  if (is_case_insensitive()) {
    expect_equal(file_real_case(files2_upper), files2_real)
    expect_equal(file_real_case(files2_lower), files2_real)
  }
})

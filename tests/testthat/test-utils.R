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
  file_remove("test.zip")

  ## Then, out of place:
  path <- file.path(tempdir(), "test")
  dir.create(path)
  file.copy(c("code.R", "remake.yml"), path)

  dest <- zip_dir(path)
  expect_that(dest, equals("test.zip"))
  expect_that(all(expected %in% contents$Name), is_true())
  file_remove("test.zip")
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

test_that("file_exists", {
  expect_that(file.exists("test-target.R"), is_true())
  ## This will be true on Mac and Windows; Linux and unixes will not
  ## see the file.
  is_case_insensitive <- Sys.info()[["sysname"]] %in% c("Darwin", "Windows")
  expect_that(file.exists("test-target.r"), equals(is_case_insensitive))

  expect_that(file_exists("test-target.r"), is_false())

  files <- dir(".", recursive=TRUE)
  ## There's a nicer way of doing this with sub I think.
  len <- nchar(files)
  files_lower <- paste0(substr(files, 1, len - 1),
                        tolower(substr(files, len, len)))
  files_upper <- paste0(substr(files, 1, len - 1),
                        toupper(substr(files, len, len)))
  ## Case munging sees them all:
  expect_that(file.exists(files_lower),
              equals(is_case_insensitive | files == files_lower))
  expect_that(file.exists(files_upper),
              equals(is_case_insensitive | files == files_upper))

  expect_that(file_exists(files_lower),
              equals(files == files_lower))
  expect_that(file_exists(files_upper),
              equals(files == files_upper))

  ## Mix in some nonexistant things:
  fake <- c("test/fake.r", "fakedir/fake.r", "fake.r")
  o <- sample(length(files) + length(fake))
  files2 <- c(files, fake)[o]
  files2_lower <- c(files_lower, fake)[o]
  files2_upper <- c(files_upper, fake)[o]

  exists <- !(files2 %in% fake)
  expect_that(file.exists(files2), equals(exists))
  expect_that(file_exists(files2_lower),
              equals(exists & files2 == files2_lower))
  expect_that(file_exists(files2_upper),
              equals(exists & files2 == files2_upper))

  expect_that(file_real_case(files),
              equals(files))
  if (is_case_insensitive) {
    expect_that(file_real_case(files_upper),
                equals(files))
    expect_that(file_real_case(files_lower),
                equals(files))
  }

  files2_real <- ifelse(exists, files2, NA_character_)
  expect_that(file_real_case(files2),
              equals(files2_real))
  if (is_case_insensitive) {
    expect_that(file_real_case(files2_upper),
                equals(files2_real))
    expect_that(file_real_case(files2_lower),
                equals(files2_real))
  }
})

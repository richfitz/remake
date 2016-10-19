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

test_that("caching file store", {
  lava <- function(...) stop("Can't touch this")

  filename <- tempfile()
  bytes <- as.raw(sample(0:255, 1000, TRUE))
  writeBin(bytes, filename)
  h <- digest::digest(bytes, serialize = FALSE)

  st <- file_store$new()
  expect_false(st$db$exists(filename))

  expect_equal(st$get_hash(filename),
               unname(tools::md5sum(filename)))
  expect_true(st$db$exists(filename))
  expect_equal(st$db$get(filename)$size, 1000)
  expect_equal(st$db$get(filename)$mtime, file.mtime(filename))
  expect_equal(st$db$get(filename)$hash, h)

  ## Does not call back to the underlying hash function:
  with_mock("hash_files" = lava,
            expect_equal(st$get_hash(filename), h))

  ## Let's change the mtime and confirm changes.  The time resolution
  ## here is about 1s so this is slightly awkward.  A better way here
  ## might be to use `touch(1)` to directly change the mtime.  I don't
  ## know how that would work on windows though.
  mtime <- st$db$get(filename)$mtime
  repeat {
    writeBin(bytes, filename)
    if (file.mtime(filename) > mtime) {
      break
    }
    Sys.sleep(.1)
  }

  ## This *will* call out to the hash function:
  with_mock("hash_files" = lava,
            expect_error(st$get_hash(filename), "Can't touch this"))
  expect_equal(st$get_hash(filename), h)
  expect_equal(st$db$get(filename)$mtime, file.mtime(filename))
  expect_equal(st$db$get(filename)$size, 1000)

  ## Then, we could change the file *size* and force rehashing.
  writeBin(bytes[-1], filename)
  h2 <- digest::digest(bytes[-1], serialize = FALSE)
  with_mock("hash_files" = lava,
            expect_error(st$get_hash(filename), "Can't touch this"))
  expect_equal(st$get_hash(filename), h2)
  expect_equal(st$db$get(filename)$size, 999)
  expect_equal(st$db$get(filename)$mtime, file.mtime(filename))
})

test_that("store persists across remake invokations", {
  cleanup()
  make()
  obj <- remake()
  expect_true(obj$store$files$db$exists("data.csv"))
  make("clean")
  expect_true(obj$store$files$db$exists("data.csv"))
  make("purge")
  expect_true(obj$store$files$db$exists("data.csv"))
})

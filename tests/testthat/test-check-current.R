## Tests of current-ness.  This gets a bit hairy.
context("Check currentness")

## There are several different things to check:
##   1. exists: existance of the file is all that is needed.
##   2. depends: the dependencies are up to date
##   3. code: the code is up to date
##   4. all: the code *and* dependencies are up to date
## 2-4 all require existance though: we will always try to build a
## target that is missing.

test_that("Types of levels", {
  expect_equal(check_levels(),
               c("all", "code", "depends", "exists"))
  expect_true(check_code("all"))
  expect_true(check_code("code"))
  expect_false(check_code("depends"))
  expect_false(check_code("exists"))

  expect_true(check_depends("all"))
  expect_false(check_depends("code"))
  expect_true(check_depends("depends"))
  expect_false(check_depends("exists"))
})

test_that("Manual", {
  cleanup()
  m <- remake("remake.yml")
  store <- m$store

  t <- m$targets[["data.csv"]]
  expect_false(target_is_current(t, store))
  expect_false(target_is_current(t, store, "all"))
  expect_false(target_is_current(t, store, "code"))
  expect_false(target_is_current(t, store, "depends"))
  expect_false(target_is_current(t, store, "exists"))
  expect_error(target_is_current(t, store, "invalid"),
               "check must be one of")

  target_run(t, store)
  expect_true(file.exists("data.csv"))

  ## The target at this point has not been entered into the remake
  ## database (only running with make() will do this).
  expect_false(target_is_current(t, store))
  expect_false(target_is_current(t, store, "all"))
  expect_false(target_is_current(t, store, "code"))
  expect_false(target_is_current(t, store, "depends"))
  expect_true(target_is_current(t, store, "exists"))

  remake_make(m, "data.csv")
  expect_true(target_is_current(t, store))
  expect_true(target_is_current(t, store, "all"))
  expect_true(target_is_current(t, store, "code"))
  expect_true(target_is_current(t, store, "depends"))
  expect_true(target_is_current(t, store, "exists"))

  ## Now, mess about with the code record in the database.  This is
  ## mean.
  ## Invalidate the code entry:
  entry <- m$store$db$get("data.csv")
  entry$code$functions$download_data <- character(0)
  m$store$db$set("data.csv", entry)
  expect_false(target_is_current(t, store))
  expect_false(target_is_current(t, store, "all"))
  expect_false(target_is_current(t, store, "code"))
  expect_true(target_is_current(t, store, "depends"))
  expect_true(target_is_current(t, store, "exists"))

  ## And for the depends:
  remake_make(m, "data.csv")
  entry <- m$store$db$get("data.csv")
  entry$depends <- "Any information will cause failure"
  m$store$db$set("data.csv", entry)
  expect_false(target_is_current(t, store))
  expect_false(target_is_current(t, store, "all"))
  expect_true(target_is_current(t, store, "code"))
  expect_false(target_is_current(t, store, "depends"))
  expect_true(target_is_current(t, store, "exists"))
})

test_that("In target", {
  cleanup()
  m <- remake("remake_check.yml")
  store <- m$store

  t <- m$targets[["data.csv"]]
  expect_equal(t$check, "exists")
  expect_false(target_is_current(t, store))

  target_run(t, store)
  expect_true(file.exists("data.csv"))
  expect_true(target_is_current(t, store))
  expect_false(target_is_current(t, store, "all"))

  file_remove("data.csv")
  remake_make(m, "data.csv")
  expect_true(file.exists("data.csv"))
  expect_true(target_is_current(t, store))
  expect_true(target_is_current(t, store, "all"))

  m$store$db$del("data.csv")
  expect_true(target_is_current(t, store))
  expect_false(target_is_current(t, store, "all"))
  expect_true(remake_is_current(m, "data.csv"))

  expect_false(m$store$db$exists("data.csv"))
  remake_make(m, "data.csv")
  ## Didn't make it: still not in the db!:
  expect_false(m$store$db$exists("data.csv"))

  cleanup()
})

test_that("dependency_status", {
  cleanup()
  m <- remake("remake_check.yml")
  store <- m$store

  t <- m$targets[["data.csv"]]
  expect_equal(t$check, "exists")
  expect_false(target_is_current(t, store))

  status <- dependency_status(m$targets[["processed"]], m$store)
  expect_null(status$depends, NULL)
  expect_is(status$code, "list")

  status <- dependency_status(m$targets[["data.csv"]], m$store)
  expect_null(status$depends, NULL)
  expect_null(status$code, NULL)
})

test_that("In remake", {
  cleanup()
  m <- remake("remake_check.yml")

  expect_false(remake_is_current(m, "data.csv"))
  expect_false(remake_is_current(m, "data.csv", "exists"))

  remake_make(m, "plot.pdf")

  expect_true(remake_is_current(m, "data.csv"))
  expect_true(remake_is_current(m, "data.csv", "exists"))

  expect_true(remake_is_current(m, "data.csv"))
  expect_true(remake_is_current(m, "data.csv", "exists"))
  expect_true(m$store$db$exists("data.csv"))

  expect_true(remake_is_current(m, "processed"))
  expect_true(remake_is_current(m, "processed", "exists"))

  expect_true(remake_is_current(m, "plot.pdf"))
  expect_true(remake_is_current(m, "plot.pdf", "exists"))
})

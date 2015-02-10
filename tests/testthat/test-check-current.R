## Tests of current-ness.  This gets a bit hairy.
if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-remake.R")
}

context("Check currentness")

## There are several different things to check:
##   1. exists: existance of the file is all that is needed.
##   2. depends: the dependencies are up to date
##   3. code: the code is up to date
##   4. all: the code *and* dependencies are up to date
## 2-4 all require existance though: we will always try to build a
## target that is missing.

test_that("Types of levels", {
  expect_that(check_levels(),
              equals(c("all", "code", "depends", "exists")))
  expect_that(check_code("all"),     is_true())
  expect_that(check_code("code"),    is_true())
  expect_that(check_code("depends"), is_false())
  expect_that(check_code("exists"),  is_false())

  expect_that(check_depends("all"),     is_true())
  expect_that(check_depends("code"),    is_false())
  expect_that(check_depends("depends"), is_true())
  expect_that(check_depends("exists"),  is_false())
})

test_that("Manual", {
  cleanup()
  m <- remake("remake.yml")
  store <- m$store

  t <- m$targets[["data.csv"]]
  expect_that(target_is_current(t, store),          is_false())
  expect_that(target_is_current(t, store, "all"),     is_false())
  expect_that(target_is_current(t, store, "code"),    is_false())
  expect_that(target_is_current(t, store, "depends"), is_false())
  expect_that(target_is_current(t, store, "exists"),  is_false())
  expect_that(target_is_current(t, store, "invalid"),
              throws_error("check must be one of"))

  target_run(t, store)
  expect_that(file.exists("data.csv"), is_true())

  ## The target at this point has not been entered into the remake
  ## database (only running with make() will do this).
  expect_that(target_is_current(t, store),          is_false())
  expect_that(target_is_current(t, store, "all"),     is_false())
  expect_that(target_is_current(t, store, "code"),    is_false())
  expect_that(target_is_current(t, store, "depends"), is_false())
  expect_that(target_is_current(t, store, "exists"),  is_true())

  remake_make(m, "data.csv")
  expect_that(target_is_current(t, store),          is_true())
  expect_that(target_is_current(t, store, "all"),     is_true())
  expect_that(target_is_current(t, store, "code"),    is_true())
  expect_that(target_is_current(t, store, "depends"), is_true())
  expect_that(target_is_current(t, store, "exists"),  is_true())

  ## Now, mess about with the code record in the database.  This is
  ## mean.
  ## Invalidate the code entry:
  entry <- m$store$db$get("data.csv")
  entry$code$functions$download_data <- character(0)
  m$store$db$set("data.csv", entry)
  expect_that(target_is_current(t, store),          is_false())
  expect_that(target_is_current(t, store, "all"),     is_false())
  expect_that(target_is_current(t, store, "code"),    is_false())
  expect_that(target_is_current(t, store, "depends"), is_true())
  expect_that(target_is_current(t, store, "exists"),  is_true())

  ## And for the depends:
  remake_make(m, "data.csv")
  entry <- m$store$db$get("data.csv")
  entry$depends <- "Any information will cause failure"
  m$store$db$set("data.csv", entry)
  expect_that(target_is_current(t, store),          is_false())
  expect_that(target_is_current(t, store, "all"),     is_false())
  expect_that(target_is_current(t, store, "code"),    is_true())
  expect_that(target_is_current(t, store, "depends"), is_false())
  expect_that(target_is_current(t, store, "exists"),  is_true())
})

test_that("In target", {
  cleanup()
  m <- remake("remake_check.yml")
  store <- m$store

  t <- m$targets[["data.csv"]]
  expect_that(t$check, equals("exists"))
  expect_that(target_is_current(t, store), is_false())

  target_run(t, store)
  expect_that(file.exists("data.csv"), is_true())
  expect_that(target_is_current(t, store),      is_true())
  expect_that(target_is_current(t, store, "all"), is_false())

  file_remove("data.csv")
  remake_make(m, "data.csv")
  expect_that(file.exists("data.csv"), is_true())
  expect_that(target_is_current(t, store),          is_true())
  expect_that(target_is_current(t, store, "all"),     is_true())

  m$store$db$del("data.csv")
  expect_that(target_is_current(t, store),      is_true())
  expect_that(target_is_current(t, store, "all"), is_false())
  expect_that(is_current("data.csv", m), is_true())

  expect_that(m$store$db$contains("data.csv"), is_false())
  remake_make(m, "data.csv")
  ## Didn't make it: still not in the db!:
  expect_that(m$store$db$contains("data.csv"), is_false())

  cleanup()
})

test_that("dependency_status", {
  cleanup()
  m <- remake("remake_check.yml")
  store <- m$store

  t <- m$targets[["data.csv"]]
  expect_that(t$check, equals("exists"))
  expect_that(target_is_current(t, store), is_false())

  status <- dependency_status(m$targets[["processed"]], m$store)
  expect_that(status$depends, equals(NULL))
  expect_that(status$code, not(equals(NULL)))

  status <- dependency_status(m$targets[["data.csv"]], m$store)
  expect_that(status$depends, equals(NULL))
  expect_that(status$code, equals(NULL))
})

test_that("In remake", {
  cleanup()
  m <- remake("remake_check.yml")

  expect_that(is_current("data.csv", m),           is_false())
  expect_that(is_current("data.csv", m, "exists"), is_false())

  remake_make(m, "plot.pdf")

  expect_that(is_current("data.csv", m), is_true())
  expect_that(is_current("data.csv", m, "exists"), is_true())

  expect_that(is_current("data.csv", m),           is_true())
  expect_that(is_current("data.csv", m, "exists"), is_true())
  expect_that(m$store$db$contains("data.csv"),     is_true())

  expect_that(is_current("processed", m),           is_true())
  expect_that(is_current("processed", m, "exists"), is_true())

  expect_that(is_current("plot.pdf", m),            is_true())
  expect_that(is_current("plot.pdf", m, "exists"),  is_true())
})

test_that("is_current with defaults", {
  cleanup()
  m <- remake("remake.yml")
  expect_that(is_current("plot.pdf"), is_false())
  remake_make(m, "plot.pdf")
  expect_that(is_current("plot.pdf"), is_true())
})

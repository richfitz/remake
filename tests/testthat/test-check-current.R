## Tests of current-ness.  This gets a bit hairy.
if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
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
  m <- maker$new("maker.yml")
  m$load_sources()

  t <- m$get_target("data.csv")
  expect_that(t$is_current(),          is_false())
  expect_that(t$is_current("all"),     is_false())
  expect_that(t$is_current("code"),    is_false())
  expect_that(t$is_current("depends"), is_false())
  expect_that(t$is_current("exists"),  is_false())
  expect_that(t$is_current("invalid"),
              throws_error("check must be one of"))

  t$run()
  expect_that(file.exists("data.csv"), is_true())

  ## The target at this point has not been entered into the maker
  ## database (only running with make() will do this).
  expect_that(t$is_current(),          is_false())
  expect_that(t$is_current("all"),     is_false())
  expect_that(t$is_current("code"),    is_false())
  expect_that(t$is_current("depends"), is_false())
  expect_that(t$is_current("exists"),  is_true())

  m$make("data.csv")
  expect_that(t$is_current(),          is_true())
  expect_that(t$is_current("all"),     is_true())
  expect_that(t$is_current("code"),    is_true())
  expect_that(t$is_current("depends"), is_true())
  expect_that(t$is_current("exists"),  is_true())

  ## Now, mess about with the code record in the database.  This is
  ## mean.
  ## Invalidate the code entry:
  entry <- m$store$db$get("data.csv")
  entry$code$functions$download_data <- character(0)
  m$store$db$set("data.csv", entry)
  expect_that(t$is_current(),          is_false())
  expect_that(t$is_current("all"),     is_false())
  expect_that(t$is_current("code"),    is_false())
  expect_that(t$is_current("depends"), is_true())
  expect_that(t$is_current("exists"),  is_true())

  ## And for the depends:
  m$make("data.csv")
  entry <- m$store$db$get("data.csv")
  entry$depends <- "Any information will cause failure"
  m$store$db$set("data.csv", entry)
  expect_that(t$is_current(),          is_false())
  expect_that(t$is_current("all"),     is_false())
  expect_that(t$is_current("code"),    is_true())
  expect_that(t$is_current("depends"), is_false())
  expect_that(t$is_current("exists"),  is_true())
})

test_that("In target", {
  cleanup()
  m <- maker$new("maker_check.yml")
  m$load_sources()

  t <- m$get_target("data.csv")
  expect_that(t$check, equals("exists"))
  expect_that(t$is_current(), is_false())

  t$run()
  expect_that(file.exists("data.csv"), is_true())
  expect_that(t$is_current(),      is_true())
  expect_that(t$is_current("all"), is_false())

  file_remove("data.csv")
  m$make("data.csv")
  expect_that(file.exists("data.csv"), is_true())
  expect_that(t$is_current(),          is_true())
  expect_that(t$is_current("all"),     is_true())

  m$store$db$del("data.csv")
  expect_that(t$is_current(),      is_true())
  expect_that(t$is_current("all"), is_false())
  expect_that(m$is_current("data.csv"), is_true())

  expect_that(m$store$db$contains("data.csv"), is_false())
  m$make("data.csv")
  ## Didn't make it: still not in the db!:
  expect_that(m$store$db$contains("data.csv"), is_false())

  cleanup()
})

test_that("dependency_status", {
  cleanup()
  m <- maker$new("maker_check.yml")
  m$load_sources()

  t <- m$get_target("data.csv")
  expect_that(t$check, equals("exists"))
  expect_that(t$is_current(), is_false())

  status <- m$dependency_status("processed")
  expect_that(status$depends, equals(NULL))
  expect_that(status$code, not(equals(NULL)))

  status <- m$dependency_status("data.csv")
  expect_that(status$depends, equals(NULL))
  expect_that(status$code, equals(NULL))
})

test_that("In maker", {
  cleanup()
  m <- maker$new("maker_check.yml")
  m$load_sources()

  expect_that(m$is_current("data.csv"), is_false())
  expect_that(m$is_current("data.csv", "exists"), is_false())

  m$make("plot.pdf")

  expect_that(m$is_current("data.csv"), is_true())
  expect_that(m$is_current("data.csv", "exists"), is_true())

  m$expire("plot.pdf")

  expect_that(m$is_current("data.csv"), is_true())
  expect_that(m$is_current("data.csv", "exists"), is_true())
  expect_that(m$store$db$contains("data.csv"), is_true())
  expect_that(m$is_current("processed"), is_true())
  expect_that(m$is_current("processed", "exists"), is_true())
  expect_that(m$is_current("plot.pdf"), is_false())
  expect_that(m$is_current("plot.pdf", "exists"), is_true())

  m$expire("plot.pdf", recursive=TRUE)

  expect_that(m$is_current("data.csv"), is_true())
  expect_that(m$is_current("data.csv", "exists"), is_true())
  expect_that(m$store$db$contains("data.csv"), is_false())
  expect_that(m$is_current("processed"), is_false())
  expect_that(m$is_current("processed", "exists"), is_true())
  expect_that(m$is_current("plot.pdf"), is_false())
  expect_that(m$is_current("plot.pdf", "exists"), is_true())

  m$make("plot.pdf", check="exists")

  ## Did not make anything:
  expect_that(m$is_current("data.csv"), is_true())
  expect_that(m$is_current("data.csv", "exists"), is_true())
  expect_that(m$store$db$contains("data.csv"), is_false())
  expect_that(m$is_current("processed"), is_false())
  expect_that(m$is_current("processed", "exists"), is_true())
  expect_that(m$is_current("plot.pdf"), is_false())
  expect_that(m$is_current("plot.pdf", "exists"), is_true())

  m$make("plot.pdf", check="all")

  ## Remade everything:
  expect_that(m$is_current("data.csv"), is_true())
  expect_that(m$is_current("data.csv", "exists"), is_true())
  expect_that(m$store$db$contains("data.csv"), is_true())
  expect_that(m$is_current("processed"), is_true())
  expect_that(m$is_current("processed", "exists"), is_true())
  expect_that(m$is_current("plot.pdf"), is_true())
  expect_that(m$is_current("plot.pdf", "exists"), is_true())
})

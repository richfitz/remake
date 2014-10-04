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
test_that("manual", {
  devtools::load_all("../../")

  cleanup()
  m <- maker$new("maker.yml")
  m$load_sources(FALSE)

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

if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

source_from_text <- function(src) {
  dest <- tempfile()
  writeLines(src, dest)
  on.exit(file.remove(dest))
  e <- new.env(parent=.GlobalEnv)
  source(dest, e)
  e
}

context("Script")

test_that("Build works", {
  cleanup()
  m <- maker$new("maker.yml")
  src <- m$script()
  expect_that(src, is_a("character"))
  dest <- tempfile()
  writeLines(src, dest)
  e <- new.env(parent=.GlobalEnv)
  source(dest, e)
  expect_that(ls(e), equals("processed"))
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("Build works with plotting target", {
  cleanup()
  m <- maker$new("maker3.yml")
  src <- m$script()
  expect_that(src, is_a("character"))
  e <- new.env(parent=.GlobalEnv)
  dest <- tempfile()
  writeLines(src, dest)
  source(dest, e)
  expect_that(ls(e), equals("processed"))
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

if (FALSE) {
test_that("Chained targets", {
  cleanup()
  m <- maker$new("chain.yml")

  src <- m$script("chained")
  e <- source_from_text(src)
  expect_that(ls(e), equals("chained"))
  expect_that(e$chained, equals(6))

  src <- m$script("manual")
  e <- source_from_text(src)
  expect_that(ls(e), equals(c("manual", "manual_pt1", "manual_pt2")))
  expect_that(e$manual, equals(6))
  cleanup()
})
}

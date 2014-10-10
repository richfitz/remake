if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Script")

test_that("Build works", {
  cleanup()
  m <- maker$new("maker.yml")
  src <- m$script()
  expect_that(src, is_a("maker_script"))
  expect_that(unclass(src), is_a("character"))

  expect_that(print(src), prints_text("download_data"))

  e <- source_maker_script(src, envir=new.env(parent=.GlobalEnv))

  expect_that(ls(e), equals("processed"))
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("Build works with plotting target", {
  cleanup()
  m <- maker$new("maker3.yml")
  src <- m$script()
  expect_that(src, is_a("maker_script"))
  expect_that(unclass(src), is_a("character"))

  e <- source_maker_script(src, envir=new.env(parent=.GlobalEnv))
  expect_that(ls(e), equals("processed"))
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("Simple interface", {
  cleanup()
  src <- make_script()
  cmp <- maker$new()$script()
  expect_that(src, is_identical_to(cmp))
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
  e <- source_maker_script(src, envir=new.env(parent=.GlobalEnv))
  expect_that(ls(e), equals(c("manual", "manual_pt1", "manual_pt2")))
  expect_that(e$manual, equals(6))
  cleanup()
})
}

if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Knitr")

test_that("Build works", {
  cleanup()
  m <- maker$new("knitr.yml")
  m$make("knitr.md")
  expect_that(file.exists("knitr.md"), is_true())
  expect_that(is_directory("figure"), is_true())
  cleanup()
})

test_that("Script", {
  cleanup()
  m <- maker$new("knitr.yml")
  src <- m$script("knitr.md")
  expect_that(last(src), equals('knitr::knit("knitr.Rmd", "knitr.md")'))
})

if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Package sources")

## This will get fleshed out over time, but doing this without
## creating a horrible pile of dependencies is tricky.  I notice that
## devtools doesn't actually test installation.
test_that("sources", {
  dat <- read_maker_packages("maker_packages.yml")
  expect_that(dat$sowsear$repo, equals("richfitz/sowsear"))
})

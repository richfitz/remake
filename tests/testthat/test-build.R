## So now we're ready to start the full build.  The basic idea is:

## 1. Load the configuration
## 2. Determine the target traversal order, regardless of which tasks
## are up to date.
## 3. Filter the graph by the current target we're being asked to look
## at

## Then the question is: do we rebuild everything?  Or not?  make does
## this by traversing down the tree flagging things as unclean or
## not.  But I don't particularly like that approach because if a
## lower level thing has changed we *want* to rebuild everything.  So
## I think that what should happen is that we always traverse the full
## tree, and then orchestrate different levels of logging where we'll
## print up-to-date vs changed information.

## For filter: algorithm:
## 1. Logical vector 'seen' same length as all targets, all FALSE
## 2. Start at start node, flagging TRUE.  Descend to all dependencies
## that have not been seen yet, repeat.
## 3. Subset toplogical sort by this vector.

## TODO: Take special care to pick up the NULL targets, as with
## config2.yml

if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Build")

test_that("Build works", {
  cleanup()
  m <- maker$new("config.yml")
  m$make("plot.pdf", dry_run=TRUE)
  m$make("plot.pdf", dry_run=FALSE)
  m$make("plot.pdf", dry_run=FALSE)
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("Cleanup works", {
  cleanup()
  m <- maker$new("config.yml")
  m$make("plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())
  m$cleanup()
  expect_that(file.exists("data.csv"), is_true())
  expect_that(file.exists("plot.pdf"), is_false())
  m$cleanup("deepclean")
  expect_that(file.exists("data.csv"), is_false())
})

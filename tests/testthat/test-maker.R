if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Loading makerfiles")

test_that("Recursive makerfiles", {
  expect_that(maker("recursive1.yml"),
              throws_error("Recursive include detected"))
  expect_that(maker("recursive2.yml"),
              throws_error("Recursive include detected"))
  expect_that(maker("recursive3.yml"),
              throws_error("Recursive include detected"))
})

test_that("Target/rule clash", {
  cleanup()
  expect_that(m <- maker("maker_rule_clash.yml"),
              gives_warning("Rule name clashes with target name"))
  m$make()
  expect_that(file.exists("plot.pdf"), is_true())
})

test_that("Quoting", {
  template <- '
sources:
  - code.R

targets:
  data.csv:
    command: download_data(%s)
    cleanup_level: purge

  processed:
    command: process_data(%s)

  plot.pdf:
    command: myplot(%s)
    plot: true
'
  ## First the OK version:
  filename <- "tmp_quoting.yml"
  on.exit(file.remove(filename))

  writeLines(sprintf(template, 'target_name', '"data.csv"', 'processed'),
             filename)
  expect_that(maker(filename), not(throws_error()))

  writeLines(sprintf(template, '"data.csv"', '"data.csv"', 'processed'),
             filename)
  expect_that(maker(filename), not(throws_error()))

  ## Then different errors:
  ## The first two are *useless* errors
  writeLines(sprintf(template, '"target_name"', '"data.csv"', 'processed'),
             filename)
  expect_that(maker(filename),
              throws_error("target_name must not be quoted"))

  writeLines(sprintf(template, 'data.csv', '"data.csv"', 'processed'),
             filename)
  expect_that(maker(filename),
              throws_error("target name must be quoted"))

  writeLines(sprintf(template, 'target_name', 'data.csv', 'processed'),
             filename)
  expect_that(maker(filename),
              throws_error("Incorrect quotation in target"))
  expect_that(maker(filename),
              throws_error("Should be quoted: data.csv"))

  writeLines(sprintf(template, 'target_name', '"data.csv"', '"processed"'),
             filename)
  expect_that(maker(filename),
              throws_error("Incorrect quotation in target"))
  expect_that(maker(filename),
              throws_error("Should not be quoted: processed"))
})

test_that("Verbosity", {
  default <- maker_verbose()
  expect_true(default$print_progress)
  expect_true(default$print_noop)
  expect_true(default$print_command)
  expect_true(default$print_command_abbreviate)
  expect_null(default$quiet_target)

  quiet <- maker_verbose(FALSE)
  expect_false(quiet$print_progress)
  expect_true(quiet$print_noop)
  expect_true(quiet$print_command)
  expect_true(quiet$print_command_abbreviate)
  expect_null(quiet$quiet_target)

  x <- maker_verbose(default)
  expect_identical(x, default)

  expect_error(maker_verbose(1), "verbose must be logical")
})

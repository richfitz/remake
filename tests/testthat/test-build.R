context("Build")

test_that("Build works", {
  cleanup()
  m <- remake("remake.yml")

  remake_make(m, "plot.pdf")
  expect_true(file.exists("plot.pdf"))
  remake_make(m, "plot.pdf")
  expect_true(file.exists("plot.pdf"))
  cleanup()
})

test_that("Cleanup works", {
  cleanup()
  m <- remake("remake.yml")
  remake_make(m, "plot.pdf")
  expect_true(file.exists("plot.pdf"))

  remake_make(m, "clean")
  ## Checks that clean runs a hook:
  expect_message(remake_make(m, "clean"),
                 "running post-cleanup hook")
  expect_true(file.exists("data.csv"))
  expect_false(file.exists("plot.pdf"))

  ## Tidy won't run the hook:
  msg <- capture_messages(remake_make(m, "tidy"))
  expect_false(any(grepl("running post-cleanup hook", msg)))
  ## Purge will run the hook because it depends on clean
  expect_message(remake_make(m, "purge"),
                 "running post-cleanup hook")
  expect_false(file.exists("data.csv"))
})

test_that("Fake targets", {
  cleanup()
  m <- remake("remake.yml")
  expect_false(remake_is_current(m, "data.csv"))
  expect_false(remake_is_current(m, "processed"))
  expect_false(remake_is_current(m, "plot.pdf"))
  remake_make(m, "all")
  expect_true(remake_is_current(m, "data.csv"))
  expect_true(remake_is_current(m, "processed"))
  expect_true(remake_is_current(m, "plot.pdf"))
  cleanup()
})

test_that("Depending on a file we don't make", {
  ## Manually run the download step from before -- now we have a file
  ## that remake wants to depend on, but does not generate:
  e <- new.env()
  source("code.R", e)
  e$download_data("data.csv")
  expect_true(file.exists("data.csv"))
  m <- remake("remake2.yml")
  expect_false(file.exists("plot.pdf"))
  expect_error(remake_make(m, "plot.pdf"), NA)
  expect_true(file.exists("plot.pdf"))
  remake_make(m, "purge")
  expect_true(file.exists("data.csv"))
  expect_false(file.exists("plot.pdf"))
  cleanup()
})

test_that("Error in source file", {
  cleanup()
  writeLines(c(readLines("code.R"), "}"), "code2.R")
  m <- remake()
  ## Ugly, and might not work in future:
  m$store$env$sources <- "code2.R"
  expect_error(m$store$env$reload(),
               "while sourcing 'code2.R'")
  cleanup()
})

test_that("Error in yaml", {
  cleanup()
  expect_error(m <- remake("nonexistant.yml"),
               "'nonexistant.yml' does not exist")
  writeLines(sub("command: download_data", " command: download_data",
                 readLines("remake.yml")),
             "remake_error.yml")

  expect_error(remake("remake_error.yml"),
               "while reading 'remake_error.yml'")
  cleanup()
})

test_that("simple interface", {
  cleanup()
  make()
  expect_true(file.exists("plot.pdf"))
  cleanup()
})

test_that("extra dependencies", {
  cleanup()
  m <- remake("remake_extra_deps.yml")
  remake_make(m, "processed")
  expect_true(file.exists("data.csv"))
})

context("Build")

test_that("Build works", {
  cleanup()
  m <- remake("remake.yml")

  remake_make(m, "plot.pdf", dry_run=TRUE)
  remake_make(m, "plot.pdf", dry_run=FALSE)
  remake_make(m, "plot.pdf", dry_run=FALSE)
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("Cleanup works", {
  cleanup()
  m <- remake("remake.yml")
  remake_make(m, "plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())

  remake_make(m, "clean")
  ## Checks that clean runs a hook:
  expect_that(remake_make(m, "clean"),
              shows_message("running post-cleanup hook"))
  expect_that(file.exists("data.csv"), is_true())
  expect_that(file.exists("plot.pdf"), is_false())

  ## Tidy won't run the hook:
  expect_that(remake_make(m, "tidy"),
              not(shows_message("running post-cleanup hook")))
  ## Purge will run the hook because it depends on clean
  expect_that(remake_make(m, "purge"),
              shows_message("running post-cleanup hook"))
  expect_that(file.exists("data.csv"), is_false())
})

test_that("Fake targets", {
  cleanup()
  m <- remake("remake.yml")
  expect_that(is_current("data.csv",  m), is_false())
  expect_that(is_current("processed", m), is_false())
  expect_that(is_current("plot.pdf",  m), is_false())
  remake_make(m, "all")
  expect_that(is_current("data.csv",  m), is_true())
  expect_that(is_current("processed", m), is_true())
  expect_that(is_current("plot.pdf",  m), is_true())
  cleanup()
})

test_that("Depending on a file we don't make", {
  ## Manually run the download step from before -- now we have a file
  ## that remake wants to depend on, but does not generate:
  e <- new.env()
  source("code.R", e)
  e$download_data("data.csv")
  expect_that(file.exists("data.csv"), is_true())
  m <- remake("remake2.yml")
  expect_that(file.exists("plot.pdf"), is_false())
  expect_that(remake_make(m, "plot.pdf"), not(throws_error()))
  expect_that(file.exists("plot.pdf"), is_true())
  remake_make(m, "purge")
  expect_that(file.exists("data.csv"), is_true())
  expect_that(file.exists("plot.pdf"), is_false())
  cleanup()
})

test_that("Error in source file", {
  cleanup()
  writeLines(c(readLines("code.R"), "}"), "code2.R")
  m <- remake()
  ## Ugly, and might not work in future:
  m$store$env$sources <- "code2.R"
  expect_that(m$store$env$reload(TRUE),
              throws_error("while sourcing 'code2.R'"))
  cleanup()
})

test_that("Error in yaml", {
  cleanup()
  expect_that(m <- remake("nonexistant.yml"),
              throws_error("'nonexistant.yml' does not exist"))
  writeLines(sub("command: download_data", " command: download_data",
                 readLines("remake.yml")),
             "remake_error.yml")

  expect_that(remake("remake_error.yml"),
              throws_error("while reading 'remake_error.yml'"))
  cleanup()
})

test_that("simple interface", {
  cleanup()
  make()
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("extra dependencies", {
  cleanup()
  m <- remake("remake_extra_deps.yml")
  remake_make(m, "processed")
  expect_that(file.exists("data.csv"), is_true())
})

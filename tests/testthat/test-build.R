if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Build")

test_that("Build works", {
  cleanup()
  m <- maker$new("maker.yml")

  m$make("plot.pdf", dry_run=TRUE)

  m$make("plot.pdf", dry_run=FALSE)
  m$make("plot.pdf", dry_run=FALSE)
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("Cleanup works", {
  cleanup()
  m <- maker$new("maker.yml")
  m$make("plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())

  m$make("clean")
  ## Checks that clean runs a hook:
  expect_that(m$make("clean"), shows_message("running post-cleanup hook"))
  expect_that(file.exists("data.csv"), is_true())
  expect_that(file.exists("plot.pdf"), is_false())

  ## Tidy won't run the hook:
  expect_that(m$make("tidy"), not(shows_message("running post-cleanup hook")))
  ## Purge will run the hook because it depends on clean
  expect_that(m$make("purge"), shows_message("running post-cleanup hook"))
  expect_that(file.exists("data.csv"), is_false())
})

test_that("Fake targets", {
  cleanup()
  m <- maker$new("maker.yml")
  expect_that(m$is_current("data.csv"), is_false())
  expect_that(m$is_current("processed"), is_false())
  expect_that(m$is_current("plot.pdf"), is_false())
  m$make("all")
  expect_that(m$is_current("data.csv"),  is_true())
  expect_that(m$is_current("processed"), is_true())
  expect_that(m$is_current("plot.pdf"),  is_true())
  cleanup()
})

test_that("Depending on a file we don't make", {
  ## Manually run the download step from before -- now we have a file
  ## that maker wants to depend on, but does not generate:
  e <- new.env()
  source("code.R", e)
  e$download_data("data.csv")
  expect_that(file.exists("data.csv"), is_true())
  m <- maker$new("maker2.yml")
  expect_that(file.exists("plot.pdf"), is_false())
  expect_that(m$make("plot.pdf"), not(throws_error()))
  expect_that(file.exists("plot.pdf"), is_true())
  m$make("purge")
  expect_that(file.exists("data.csv"), is_true())
  expect_that(file.exists("plot.pdf"), is_false())
  cleanup()
})

test_that("Expiring targets", {
  cleanup()
  m <- maker$new()
  m$make("plot.pdf")

  ## Sanity check:
  expect_that(file.exists("data.csv"), is_true())
  expect_that(m$is_current("data.csv"), is_true())
  expect_that(file.exists("plot.pdf"), is_true())
  expect_that(m$is_current("plot.pdf"), is_true())

  m$expire("plot.pdf")
  ## Still there:
  expect_that(file.exists("plot.pdf"), is_true())
  ## But not current:
  expect_that(m$is_current("plot.pdf"), is_false())

  ## Recursively expire:
  m$expire("plot.pdf", recursive=TRUE)
  expect_that(file.exists("data.csv"), is_true())
  expect_that(m$is_current("data.csv"), is_false())

  ## There's no easy way of catching this, but this has now rebuild
  ## everything, despite the files still being there.
  m$make()

  ## Works for objects as well as files:
  m$expire("processed")
  expect_that(m$is_current("processed"), is_false())
  expect_that(m$get("processed"), is_a("data.frame"))
  ## Because the *actual* R object is still there, the plot.pdf is
  ## considered current, but it would be rebuild because processed
  ## would be rebuilt.
  expect_that(m$is_current("plot.pdf"), is_true())
  m$make()

  cleanup()
})

test_that("Error in source file", {
  cleanup()
  writeLines(c(readLines("code.R"), "}"), "code2.R")
  m <- maker$new()
  ## Ugly, and might not work in future:
  m$store$env$sources <- "code2.R"
  expect_that(m$load_sources(),
              throws_error("while sourcing 'code2.R'"))
  ## Will continually throw this error even though the files have not
  ## changed:
  expect_that(m$load_sources(),
              throws_error("while sourcing 'code2.R'"))
  cleanup()
})

test_that("Error in yaml", {
  cleanup()
  expect_that(m <- maker$new("nonexistant.yml"),
              throws_error("'nonexistant.yml' does not exist"))
  writeLines(sub("rule: do_plot", " rule: do_plot", readLines("maker.yml")),
             "maker_error.yml")

  expect_that(maker$new("maker_error.yml"),
              throws_error("while reading 'maker_error.yml'"))
  cleanup()
})

test_that("Positional target_argument", {
  cleanup()
  m <- maker$new("maker_pos.yml")
  m$make()
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("command interface", {
  devtools::load_all("../../")
  cleanup()
  m <- maker$new("maker_command.yml")
  m$make()
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("simple interface", {
  cleanup()
  make()
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("make_dependencies", {
  cleanup()
  m <- maker$new("maker.yml")
  e <- m$make_dependencies("plot.pdf")
  expect_that(e, is_a("maker_environment"))

  expect_that(ls(e), equals("processed"))

  expect_that(maker_environment_attach(e),
              shows_message("Maker environment for building"))
  expect_that("maker:plot.pdf" %in% search(), is_true())
  expect_that("maker:functions" %in% search(), is_true())

  expect_that(exists("processed"), is_true())
  expect_that(processed, is_a("data.frame"))

  expect_that(maker_environment_detach(),
              shows_message("Detaching"))
  expect_that(maker_environment_detach(),
              gives_warning("No maker environments found on search path"))
  expect_that(maker_environment_detach(warn=FALSE),
              not(gives_warning()))
  expect_that(exists("processed"), is_false())

  expect_that(maker_environment_attach(e, verbose=FALSE),
              not(shows_message()))
  expect_that(exists("processed"), is_true())
  expect_that(maker_environment_detach(verbose=FALSE),
              not(shows_message()))
  expect_that(exists("processed"), is_false())
})

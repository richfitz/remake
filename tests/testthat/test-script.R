context("Script")

test_that("Build works", {
  cleanup()
  m <- remake("remake.yml")
  src <- remake_script(m)
  expect_is(unclass(src), "character")

  expect_output(print(src), "download_data")
  expect_match(src, 'library("testthat")', fixed=TRUE, all=FALSE)
  expect_match(src, 'source("code.R")', fixed=TRUE, all=FALSE)

  ## Sourcing the script will run run various "source" commands which
  ## will modify the global environment.  That's not really ideal.
  extras <- c("do_plot", "clean_hook", "download_data", "myplot",
              "process_data")
  rm_all_remake_objects <- function() {
    suppressWarnings(rm(list=c("processed", extras), envir=.GlobalEnv))
  }
  rm_all_remake_objects()
  on.exit(rm_all_remake_objects())

  ## With source rewriting:
  e <- source_character(src, envir=new.env(parent=.GlobalEnv),
                        rewrite_source=TRUE)
  expect_equal(sort(ls(e)), sort(c("processed", extras)))
  expect_true(file.exists("plot.pdf"))
  expect_false(any(exists(extras, .GlobalEnv, inherits=FALSE)))

  e <- source_character(src, envir=new.env(parent=.GlobalEnv),
                        rewrite_source=FALSE)
  expect_equal(ls(e), "processed")
  expect_true(all(exists(extras, .GlobalEnv, inherits=FALSE)))

  e <- source_character(src)
  expect_identical(e, .GlobalEnv)
  expect_true(all(c("processed", extras) %in% ls(.GlobalEnv)))

  rm_all_remake_objects()
  cleanup()
})

test_that("Build works with plotting target", {
  cleanup()
  m <- remake("plot_simple.yml")
  src <- remake_script(m)
  expect_is(unclass(src), "character")

  e <- source_character(src, envir=new.env(parent=.GlobalEnv))
  expect_true("processed" %in% ls(e))
  expect_true(file.exists("plot.pdf"))
  cleanup()
})

test_that("Simple interface", {
  cleanup()
  src <- make_script()
  cmp <- remake_script(remake())
  expect_identical(src, cmp)
})

test_that("Source directory", {
  cleanup()
  dir.create("source_dir", FALSE)
  writeLines(character(0), "source_dir/a.R")
  writeLines(character(0), "source_dir/b.R")

  str <- make_script(remake_file="source_dir.yml")
  expect_equal(str[[1]], 'source("source_dir/a.R")')
  expect_equal(str[[2]], 'source("source_dir/b.R")')
  expect_equal(str[[3]], 'all <- identity(TRUE)')
  cleanup()
})

test_that("Create directory", {
  cleanup()

  m <- remake("remake_directory.yml")
  remake_make(m, "export/plot.pdf")
  str <- make_script(remake_file="remake_directory.yml")

  expect_equal(sum(grepl("dir.create", str, fixed=TRUE)), 1)
  expect_true('dir.create("export", FALSE, TRUE)' %in% str)
})

test_that("Load extra packages", {
  cleanup()
  m <- remake("remake_target_packages.yml")
  str <- make_script(c("all", "will_load"),
                     remake_file="remake_target_packages.yml")
  expect_true('library("devtools")' %in% str)
  expect_equal(sum(str == 'library("devtools")'), 1)
  expect_true(which(str == 'library("devtools")') != 1)
})

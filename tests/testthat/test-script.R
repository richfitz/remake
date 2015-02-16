context("Script")

test_that("Build works", {
  cleanup()
  m <- remake("remake.yml")
  src <- remake_script(m)
  expect_that(unclass(src), is_a("character"))

  expect_that(print(src), prints_text("download_data"))

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
  expect_that(sort(ls(e)), equals(sort(c("processed", extras))))
  expect_that(file.exists("plot.pdf"), is_true())
  expect_that(any(exists(extras, .GlobalEnv, inherits=FALSE)),
              is_false())

  e <- source_character(src, envir=new.env(parent=.GlobalEnv),
                        rewrite_source=FALSE)
  expect_that(ls(e), equals("processed"))
  expect_that(all(exists(extras, .GlobalEnv, inherits=FALSE)),
              is_true())

  e <- source_character(src)
  expect_that(e, is_identical_to(.GlobalEnv))
  expect_that(all(c("processed", extras) %in% ls(.GlobalEnv)),
              is_true())

  rm_all_remake_objects()
  cleanup()
})

test_that("Build works with plotting target", {
  cleanup()
  m <- remake("plot_simple.yml")
  src <- remake_script(m)
  expect_that(unclass(src), is_a("character"))

  e <- source_character(src, envir=new.env(parent=.GlobalEnv))
  expect_that("processed" %in% ls(e), is_true())
  expect_that(file.exists("plot.pdf"), is_true())
  cleanup()
})

test_that("Simple interface", {
  cleanup()
  src <- make_script()
  cmp <- remake_script(remake())
  expect_that(src, is_identical_to(cmp))
})

test_that("Source directory", {
  cleanup()
  dir.create("source_dir", FALSE)
  writeLines(character(0), "source_dir/a.R")
  writeLines(character(0), "source_dir/b.R")

  str <- make_script(remake_file="source_dir.yml")
  expect_that(str[[1]], equals('source("source_dir/a.R")'))
  expect_that(str[[2]], equals('source("source_dir/b.R")'))
  expect_that(str[[3]], equals('all <- identity(TRUE)'))
  cleanup()
})

test_that("Create directory", {
  cleanup()

  m <- remake("remake_directory.yml")
  remake_make(m, "export/plot.pdf")
  str <- make_script(remake_file="remake_directory.yml")

  expect_that(sum(grepl("dir.create", str, fixed=TRUE)), equals(1))
  expect_that('dir.create("export", FALSE, TRUE)' %in% str, is_true())
})

test_that("Load extra packages", {
  cleanup()
  m <- remake("remake_target_packages.yml")
  str <- make_script(c("all", "will_load"),
                     remake_file="remake_target_packages.yml")
  expect_that('library("devtools")' %in% str, is_true())
  expect_that(sum(str == 'library("devtools")'), equals(1))
  expect_that(which(str == 'library("devtools")') != 1, is_true())
})

if (FALSE) { ## TODO -- looks like this might be failing?
test_that("Chained targets", {
  cleanup()
  m <- remake("chain.yml")

  src <- remake_script(m, "chained")
  e <- source_from_text(src)
  expect_that(ls(e), equals("chained"))
  expect_that(e$chained, equals(6))

  src <- remake_script(m, "manual")
  e <- source_character(src, envir=new.env(parent=.GlobalEnv))
  expect_that(ls(e), equals(c("manual", "manual_pt1", "manual_pt2")))
  expect_that(e$manual, equals(6))
  cleanup()
})
}

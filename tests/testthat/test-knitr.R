context("Knitr")

test_that("Defaults", {
  expect_equal(knitr::opts_chunk$get("fig.path"), "figure/")
})

test_that("Build Rmd works", {
  cleanup()
  m <- remake("knitr.yml")
  remake_make(m, "knitr.md")
  expect_true(file.exists("knitr.md"))
  expect_true(is_directory("figure"))
  expect_true(file.exists("figure/unnamed-chunk-2-1.png"))
  expect_equal(knitr::opts_chunk$get("fig.path"), "figure/")
  cleanup()
})

test_that("Build Rnw works", {
  cleanup()
  m <- remake("knitr.yml")
  ## TODO: This gives a warning because knitr is trying to look for
  ## framed.sty and doing a pretty terrible job at it.  This also
  ## produces the warning about
  ##   sh: 1: kpsewhich: not found
  ## which can't be suppressed.
  suppressWarnings(remake_make(m, "knitr.tex"))
  expect_true(file.exists("knitr.tex"))
  expect_true(is_directory("figure"))
  expect_true(file.exists("figure/unnamed-chunk-2-1.pdf"))
  expect_equal(knitr::opts_chunk$get("fig.path"), "figure/")
  cleanup()
})

test_that("Script", {
  cleanup()
  m <- remake("knitr.yml")
  src <- remake_script(m, "knitr.md")
  expect_equal(last(src), 'knitr::knit("knitr.Rmd", "knitr.md")')
})

test_that("knitr depends (file only)", {
  cleanup()
  m <- remake("knitr_file_dep.yml")
  expect_false(file.exists("data.csv"))
  remake_make(m)
  expect_true(file.exists("data.csv"))
})

test_that("knitr options", {
  cleanup()
  m <- remake("knitr_opts.yml")
  remake_make(m, "knitr.md")

  ## Check I have the default knitr options:
  expect_equal(knitr::opts_chunk$get("fig.path"), "figure/")
  expect_false(knitr::opts_chunk$get("tidy"))

  ## This should dump out figures in a special place because the
  ## fig.path option was set:
  expect_equal(dir("figure"), "myprefix_unnamed-chunk-2-1.png")

  ## But leaves the gloal options unchanged:
  expect_equal(knitr::opts_chunk$get("fig.path"), "figure/")
  expect_false(knitr::opts_chunk$get("tidy"))

  cleanup()
})

test_that("Option sets", {
  cleanup()
  m <- remake("knitr_options.yml")
  t <- m$targets[["knitr.md"]]
  dat <- yaml_read("knitr_options.yml")
  cmp <- c(dat$knitr_options$mystyle, list(input="knitr.Rmd"))
  cmp$options$error <- FALSE
  expect_equal(t$knitr, cmp)
})

test_that("auto_figure_prefix", {
  cleanup()
  m <- remake("knitr_prefix.yml")
  remake_make(m, "knitr.md")
  expect_true(file.exists("knitr.md"))
  expect_true(is_directory("figure"))
  expect_true(file.exists("figure/knitr__unnamed-chunk-2-1.png"))
  ## Knitr options reset:
  expect_equal(knitr::opts_chunk$get("fig.path"), "figure/")
  cleanup()
})

test_that("Errors during compilation propagate", {
  cleanup()
  m <- remake("knitr_error.yml")
  expect_error(remake_make(m, "knitr_rename.md"), "not subsettable")
  t <- m$targets[["knitr_rename.md"]]
  expect_false(t$knitr$options$error)
  t$knitr$options$error <- TRUE
  m$targets[["knitr_rename.md"]] <- t
  expect_error(remake_make(m, "knitr_rename.md"), NA)
})

test_that("Renaming exported objects", {
  cleanup()
  m <- remake("knitr_rename.yml")
  remake_make(m, "knitr_rename.md")
})

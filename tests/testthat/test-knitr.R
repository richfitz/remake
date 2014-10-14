if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Knitr")

test_that("Defaults", {
  expect_that(knitr::opts_chunk$get("fig.path"),
              equals("figure/"))
})

test_that("Build works", {
  cleanup()
  m <- maker$new("knitr.yml")
  m$make("knitr.md")
  expect_that(file.exists("knitr.md"), is_true())
  expect_that(is_directory("figure"), is_true())
  expect_that(file.exists("figure/unnamed-chunk-2.png"),
              is_true())
  expect_that(knitr::opts_chunk$get("fig.path"),
              equals("figure/"))
  cleanup()
})

test_that("Script", {
  cleanup()
  m <- maker$new("knitr.yml")
  src <- m$script("knitr.md")
  expect_that(last(src), equals('knitr::knit("knitr.Rmd", "knitr.md")'))
})

test_that("knitr depends", {
  cleanup()
  m <- maker$new("knitr.yml")
  t <- m$get_target("knitr.md")
  expect_that(knitr_depends(t$maker, t$depends),
              equals("processed"))

  expect_that(knitr_depends(t$maker, unname(m$get_targets("processed"))),
              equals("processed"))

  expect_that(knitr_depends(t$maker, unname(m$get_targets("all"))),
              equals("processed"))

  expect_that(knitr_depends(t$maker, list()),
              equals(character(0)))
})

test_that("knitr options", {
  cleanup()
  m <- maker$new("knitr_opts.yml")
  m$make("knitr.md")

  ## Check I have the default knitr options:
  expect_that(knitr::opts_chunk$get("fig.path"), equals("figure/"))
  expect_that(knitr::opts_chunk$get("tidy"), is_false())

  ## This should dump out figures in a special place because the
  ## fig.path option was set:
  expect_that(dir("figure"), equals("myprefix_unnamed-chunk-2.png"))

  ## But leaves the gloal options unchanged:
  expect_that(knitr::opts_chunk$get("fig.path"), equals("figure/"))
  expect_that(knitr::opts_chunk$get("tidy"), is_false())

  cleanup()
})

test_that("auto_figure_prefix", {
  cleanup()
  m <- maker$new("knitr_prefix.yml")
  m$make("knitr.md")
  expect_that(file.exists("knitr.md"), is_true())
  expect_that(is_directory("figure"), is_true())
  expect_that(file.exists("figure/knitr__unnamed-chunk-2.png"),
              is_true())
  ## Knitr options reset:
  expect_that(knitr::opts_chunk$get("fig.path"),
              equals("figure/"))
  cleanup()
})

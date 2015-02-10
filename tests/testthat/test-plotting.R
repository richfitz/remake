if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-remake.R")
}

context("Plot")

test_that("Build works", {
  cleanup()
  m <- remake("plot_simple.yml")
  remake_make(m, "plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())
  ## TODO: ideally check that it is the expected size
  cleanup()
})

test_that("Plot options", {
  cleanup()
  m <- remake("plot_options.yml")

  t1 <- m$targets[["plot1.pdf"]]
  t2 <- m$targets[["plot2.pdf"]]
  t3 <- m$targets[["plot3.pdf"]]
  t4 <- m$targets[["plot4.pdf"]]

  style <- list(width=8, height=4)

  expect_that(t1$plot$args, equals(style))
  expect_that(t2$plot$args, equals(style))

  ## No arguments on these two:
  expect_that(t3$plot$args, equals(empty_named_list()))
  expect_that(t4$plot$args, equals(empty_named_list()))

  remake_make(m)

  expect_that(file.exists("plot1.pdf"), is_true())
  expect_that(file.exists("plot2.pdf"), is_true())
  expect_that(file.exists("plot3.pdf"), is_true())
  expect_that(file.exists("plot4.pdf"), is_true())

  cleanup()
})

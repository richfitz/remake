if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Plot")

test_that("Build works", {
  cleanup()
  m <- maker$new("maker3.yml")
  m$make("plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())
  ## TODO: ideally check that it is the expected size
  cleanup()
})

test_that("Plot options", {
  cleanup()
  m <- maker$new("plot_options.yml")

  t1 <- m$get_target("plot.pdf")
  t2 <- m$get_target("plot2.pdf")
  t3 <- m$get_target("plot3.pdf")
  t4 <- m$get_target("plot4.pdf")

  style <- list(width=8, height=4)

  expect_that(m$plot_options$mystyle, equals(style))
  expect_that(t1$plot$args, equals(style))
  expect_that(t2$plot$args, equals(style))

  ## No arguments on these two:
  expect_that(t3$plot$args, equals(list()))
  expect_that(t4$plot$args, equals(list()))

  m$make()
  cleanup()
})

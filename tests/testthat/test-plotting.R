context("Plot")

test_that("Build works", {
  cleanup()
  m <- remake("plot_simple.yml")
  remake_make(m, "plot.pdf")
  expect_true(file.exists("plot.pdf"))
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

  expect_equal(t1$plot$args, style)
  expect_equal(t2$plot$args, style)

  ## No arguments on these two:
  expect_equal(t3$plot$args, empty_named_list())
  expect_equal(t4$plot$args, empty_named_list())

  remake_make(m)

  expect_true(file.exists("plot1.pdf"))
  expect_true(file.exists("plot2.pdf"))
  expect_true(file.exists("plot3.pdf"))
  expect_true(file.exists("plot4.pdf"))

  cleanup()
})

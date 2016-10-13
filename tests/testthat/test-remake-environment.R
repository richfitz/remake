context("remake_environment")

test_that("remake_environment", {
  cleanup()
  m <- remake()
  remake_make(m)
  e <- remake_environment(m, "processed")
  expect_equal(ls(e), "processed")
  expect_true(exists("do_plot", e))
  expect_equal(ls(parent.env(e)), ls(m$store$env$env))
  expect_identical(parent.env(e), m$store$env$env)

  ## Can copy sources instead:
  e <- remake_environment(m, "processed", copy_functions=TRUE)
  expect_equal(sort(ls(e)),
               sort(c("processed", ls(m$store$env$env))))
  expect_true(exists("do_plot", e))
  
  expect_false(identical(parent.env(e), m$store$env$env))
  expect_false(identical(e, m$store$env$env))

  ## In this case, depenencies has no effect:
  e <- remake_environment(m, "processed", dependencies=TRUE)
  expect_equal(ls(e), "processed")
})

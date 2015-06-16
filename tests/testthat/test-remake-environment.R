context("remake_environment")

test_that("remake_environment", {
  cleanup()
  m <- remake()
  remake_make(m)
  e <- remake_environment(m, "processed")
  expect_that(ls(e), equals("processed"))
  expect_that(exists("do_plot", e), is_true())
  expect_that(ls(parent.env(e)), equals(ls(m$store$env$env)))
  expect_that(parent.env(e), is_identical_to(m$store$env$env))

  ## Can copy sources instead:
  e <- remake_environment(m, "processed", copy_functions=TRUE)
  expect_that(sort(ls(e)),
              equals(sort(c("processed", ls(m$store$env$env)))))
  expect_that(exists("do_plot", e), is_true())
  expect_that(parent.env(e), not(is_identical_to(m$store$env$env)))
  expect_that(e, not(is_identical_to(m$store$env$env)))

  ## In this case, depenencies has no effect:
  e <- remake_environment(m, "processed", dependencies=TRUE)
  expect_that(ls(e), equals("processed"))
})

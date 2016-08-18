context("Targets")

test_that("Targets return their output on build", {
  cleanup()
  m <- remake("remake.yml")
  ## TODO: there's a really obscure error if this is not run but
  ## targets are.  Really need to catch that somewhere.
  store <- m$store

  ## File targets will invisibly return their filename:
  t <- m$targets[["data.csv"]]
  expect_equal(target_build(t, store), "data.csv")

  remake_remove_target(m, "data.csv")
  expect_equal(remake_update(m, "data.csv"), "data.csv")
  expect_equal(remake_update(m, "data.csv"), "data.csv")

  remake_remove_target(m, "data.csv")
  expect_equal(remake_make(m, "data.csv"), "data.csv")
  expect_equal(remake_make(m, "data.csv"), "data.csv")

  ## While object targets invisibly return their contents
  t <- m$targets[["processed"]]
  expect_is(target_build(t, store), "data.frame")

  remake_remove_target(m, "processed")
  expect_is(remake_update(m, "processed"), "data.frame")
  expect_is(remake_update(m, "processed"), "data.frame")

  remake_remove_target(m, "processed")
  expect_is(remake_make(m, "processed"), "data.frame")
  expect_is(remake_make(m, "processed"), "data.frame")

  ## Fake targets return nothing:
  t <- m$targets[["all"]]
  expect_null(target_build(t, store))

  expect_error(remake_remove_target(m, "all"),
               "Not something that can be deleted")
  expect_null(remake_update(m, "all"))
  expect_null(remake_update(m, "all"))

  expect_error(remake_remove_target(m, "all"),
               "Not something that can be deleted")
  expect_null(remake_make(m, "all"))
  expect_null(remake_make(m, "all"))
})

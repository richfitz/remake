context("Targets")

test_that("Targets return their output on build", {
  cleanup()
  m <- remake("remake.yml")
  ## TODO: there's a really obscure error if this is not run but
  ## targets are.  Really need to catch that somewhere.
  store <- m$store

  ## File targets will invisibly return their filename:
  t <- m$targets[["data.csv"]]
  expect_that(target_build(t, store), equals("data.csv"))

  remake_remove_target(m, "data.csv")
  expect_that(remake_update(m, "data.csv"), equals("data.csv"))
  expect_that(remake_update(m, "data.csv"), equals("data.csv"))

  remake_remove_target(m, "data.csv")
  expect_that(remake_make(m, "data.csv"), equals("data.csv"))
  expect_that(remake_make(m, "data.csv"), equals("data.csv"))

  ## While object targets invisibly return their contents
  t <- m$targets[["processed"]]
  expect_that(target_build(t, store), is_a("data.frame"))

  remake_remove_target(m, "processed")
  expect_that(remake_update(m, "processed"), is_a("data.frame"))
  expect_that(remake_update(m, "processed"), is_a("data.frame"))

  remake_remove_target(m, "processed")
  expect_that(remake_make(m, "processed"), is_a("data.frame"))
  expect_that(remake_make(m, "processed"), is_a("data.frame"))

  ## Fake targets return nothing:
  t <- m$targets[["all"]]
  expect_that(target_build(t, store), is_null())

  expect_that(remake_remove_target(m, "all"),
              throws_error("Not something that can be deleted"))
  expect_that(remake_update(m, "all"), is_null())
  expect_that(remake_update(m, "all"), is_null())

  expect_that(remake_remove_target(m, "all"),
              throws_error("Not something that can be deleted"))
  expect_that(remake_make(m, "all"), is_null())
  expect_that(remake_make(m, "all"), is_null())
})

test_that("Extensions", {
  expect_that(file_extensions(),
              is_identical_to(tolower(file_extensions())))
})

context("Target packages")

test_that("bootstrap", {
  expect_false(exists("install_github"))
  expect_false("devtools" %in% .packages())
})

test_that("target with no extra packages", {
  cleanup()
  m <- remake("remake_target_packages.yml")
  t <- m$targets[["will_not_load"]]
  expect_null(t$packages)
  x <- remake_make(m, "will_not_load")
  expect_false(x)
})

test_that("target that loads extra package", {
  cleanup()
  m <- remake("remake_target_packages.yml")
  t <- m$targets[["will_load"]]
  expect_equal(t$packages, "devtools")
  x <- remake_make(m, "will_load")
  expect_true(x)
  expect_false("devtools" %in% .packages())
})

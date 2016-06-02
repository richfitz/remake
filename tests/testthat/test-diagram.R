context("Diagrams")

test_that("Can make diagram", {
  obj <- diagram()
  expect_is(obj, "grViz")
})

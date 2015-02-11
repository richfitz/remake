context("Diagrams")

test_that("Can make diagram", {
  obj <- diagram()
  expect_that(obj, is_a("grViz"))
})

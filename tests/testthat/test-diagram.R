context("Diagrams")

test_that("Can make diagram", {
  skip_if_not_installed("DiagrammeR")
  obj <- diagram()
  expect_that(obj, is_a("grViz"))
})

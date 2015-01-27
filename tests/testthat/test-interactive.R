if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Interactive maker")

test_that("Drop braces", {
  expect_that(interactive_drop_braces(quote(a)),
              is_identical_to(quote(a)))
  expect_that(interactive_drop_braces(quote({a})),
              is_identical_to(quote(a)))
  expect_that(interactive_drop_braces(quote({{a}})),
              is_identical_to(quote(a)))

  expect_that(interactive_drop_braces(quote(a <- b)),
              is_identical_to(quote(a <- b)))
  expect_that(interactive_drop_braces(quote({a <- b})),
              is_identical_to(quote(a <- b)))
  expect_that(interactive_drop_braces(quote({{a <- b}})),
              is_identical_to(quote(a <- b)))

  ## Not sure why not identical here:
  expect_that(interactive_drop_braces(quote({a <- {b}})),
              equals(quote(a <- {b})))
  expect_that(interactive_drop_braces(quote({a <- {b; c}})),
              equals(quote(a <- {b; c})))

  expect_that(interactive_drop_braces(quote({a; b})),
              throws_error("Expected non-compound expression"))
  expect_that(interactive_drop_braces(quote({a; b; c})),
              throws_error("Expected non-compound expression"))

  expect_that(interactive_drop_braces(quote({})),
              equals(quote({})))
})

test_that("Assignment detection", {
  expect_that(interactive_check_assignment(quote(a <- b)),
              equals(list(name=quote(a), value=quote(b))))
  ## Using '=' is a bit more of a hassle:
  tmp <- interactive_check_assignment(interactive_drop_braces(quote({a = b})))
  expect_that(tmp, equals(list(name=quote(a), value=quote(b))))

  expect_that(interactive_check_assignment(quote(1 <- b)),
              throws_error("Invalid target of assignment"))
  expect_that(interactive_check_assignment(quote({})),
              throws_error("Expected assignment operation"))
  expect_that(interactive_check_assignment(quote(-a)),
              throws_error("Expected assignment operation"))
  expect_that(interactive_check_assignment(quote(b-a)),
              throws_error("Expected assignment operation"))
})

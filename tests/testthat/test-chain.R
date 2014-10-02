if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Chained rules")

if (FALSE) { # Disabled until rewritten
test_that("Chained rules", {
  cleanup()
  m <- maker$new("chain.yml")

  res <- m$make("manual")
  expect_that(res, equals(6))

  ## There are two chain rules created:
  expect_that(c("chained{1}", "chained{2}") %in% m$target_names(),
              equals(c(FALSE, FALSE)))
  expect_that(c("chained{1}", "chained{2}") %in% m$target_names(all=TRUE),
              equals(c(TRUE, TRUE)))

  t1 <- m$get_target("chained{1}")
  expect_that(t1, is_a("target"))
  expect_that(is.null(t1$chain_parent), is_false())
  expect_that(t1$depends, equals(list()))

  t2 <- m$get_target("chained{2}")
  expect_that(t2, is_a("target"))
  expect_that(is.null(t2$chain_parent), is_false())
  expect_that(t2$depends, equals(list(t1)))

  res <- m$make("chained")
  expect_that(res, equals(6))

  expect_that(m$store$objects$contains("chained"), is_true())
  expect_that(m$store$objects$contains("chained{1}"), is_true())
  expect_that(m$store$objects$contains("chained{2}"), is_true())

  ## Cleaning removes them all:
  m$make("clean")
  expect_that(m$store$objects$contains("chained"), is_false())
  expect_that(m$store$objects$contains("chained{1}"), is_false())
  expect_that(m$store$objects$contains("chained{2}"), is_false())

  ## By default, so does remove_target:
  res <- m$make("chained")
  m$remove_target("chained")
  expect_that(m$store$objects$contains("chained"), is_false())
  expect_that(m$store$objects$contains("chained{1}"), is_false())
  expect_that(m$store$objects$contains("chained{2}"), is_false())

  res <- m$make("chained")
  m$remove_target("chained", FALSE)
  expect_that(m$store$objects$contains("chained"), is_false())
  expect_that(m$store$objects$contains("chained{1}"), is_true())
  expect_that(m$store$objects$contains("chained{2}"), is_true())
})
}

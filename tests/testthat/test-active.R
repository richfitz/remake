if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Active binding functions")

test_that("Create active bindings", {
  cleanup()
  m <- maker::maker()
  e <- new.env()
  maker_set_active_bindings(m, e)

  expect_that(ls(e), equals("processed"))
  expect_that(d <- e$processed,
              shows_message("BUILD"))
  expect_that(d, is_a("data.frame"))
  expect_that(d <- e$processed,
              not(shows_message("BUILD")))
  expect_that(d, is_a("data.frame"))

  expect_that(e$processed <- 1,
              throws_error("read-only"))
  expect_that(bindingIsActive(quote(processed), e),
              is_true())
  ## Can be is_active_binding if my PR to pryr goes through:
  expect_that(bindingIsActive(quote(d), environment()),
              is_false())

  expect_that(maker_list_active_bindings(m, e),
              equals("processed"))
  expect_that(maker_list_active_bindings(m, e, TRUE),
              equals("processed"))
})

test_that("Delete active bindings", {
  cleanup()
  m <- maker::maker()
  e <- new.env()
  maker_set_active_bindings(m, e)

  del <- maker_delete_active_bindings(m, e)
  expect_that(del, equals("processed"))
  expect_that(ls(e), equals(character(0)))
})

test_that("Resolve active bindings", {
  cleanup()
  m <- maker::maker()
  e <- new.env()
  maker_set_active_bindings(m, e)

  expect_that(res <- maker_resolve_active_bindings(m, e),
              shows_message("BUILD"))
  expect_that(res, equals("processed"))
  expect_that(ls(e), equals("processed"))
  expect_that(bindingIsActive("processed", e), is_false())
  expect_that(e$processed, is_a("data.frame"))
})

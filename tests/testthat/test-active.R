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
  maker_set_active_bindings(m, "target", e)
  ## Replacing bindings is fine:
  expect_that(maker_set_active_bindings(m, "target", e),
              not(throws_error()))

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

  expect_that(m$active_bindings$target, equals("processed"))
  expect_that(filter_active_bindings(ls(e), e),
              equals("processed"))

  e <- new.env()
  e$processed <- 1
  expect_that(maker_set_active_bindings(m, "target", e),
              throws_error("Bindngs would overwrite normal variables"))
  expect_that(bindingIsActive("processed", e), is_false())
  expect_that(maker_set_active_bindings(m, "target", e, force=TRUE),
              not(throws_error()))
  expect_that(bindingIsActive("processed", e), is_true())
})

test_that("Delete active bindings", {
  cleanup()
  m <- maker::maker()
  e <- new.env()
  maker_set_active_bindings(m, "target", e)

  del <- maker_delete_active_bindings(m, "target", e)
  expect_that(del, equals("processed"))
  expect_that(ls(e), equals(character(0)))
})

test_that("Resolve active bindings", {
  cleanup()
  m <- maker::maker()

  e <- new.env()
  maker_set_active_bindings(m, "target", e)

  expect_that(res <- maker_resolve_active_bindings(m, "target", e),
              shows_message("SKIP"))
  expect_that(res, equals("processed"))
  expect_that(ls(e), equals("processed"))
  expect_that(bindingIsActive("processed", e), is_false())
  expect_that(e$processed, is_null())

  e <- new.env()
  maker_set_active_bindings(m, "target", e)
  expect_that(res <- maker_resolve_active_bindings(m, "target", e, force=TRUE),
              shows_message("BUILD"))

  expect_that(res, equals("processed"))
  expect_that(ls(e), equals("processed"))
  expect_that(bindingIsActive("processed", e), is_false())
  expect_that(e$processed, is_a("data.frame"))
})

test_that("Active sources", {
  cleanup()
  m <- maker::maker()
  m$load_sources()
  e <- new.env()
  maker_set_active_bindings(m, "source", e)

  expect_that(ls(e), equals(ls(m$store$env$env)))

  expect_that(e$process_data,
              is_identical_to(m$store$env$env$process_data))
  expect_that(m$active_bindings$source,
              equals(ls(e)))

  ## Then, change a function:
  m$store$env$env$process_data <- identity
  expect_that(e$process_data, is_identical_to(identity))

  expect_that(e$process_data <- identity,
              throws_error("read-only"))
})

test_that("Global mode", {
  cleanup()
  e <- new.env()
  m <- maker::maker(envir=e)
  expect_that(exists("processed", e), is_true())
  expect_that(exists("download_data", e), is_false())
  m$load_sources()
  expect_that(exists("processed", e), is_true())
  expect_that(exists("download_data", e), is_true())
  expect_that(bindingIsActive("processed", e), is_true())
  expect_that(bindingIsActive("download_data", e), is_true())

  maker_delete_active_bindings(m, "target", e)
  maker_delete_active_bindings(m, "source", e)
  expect_that(exists("processed", e), is_false())
  expect_that(exists("download_data", e), is_false())
})

if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Active binding functions")

test_that("Create active bindings", {
  cleanup()
  m <- maker::maker(envir=new.env())
  obj <- maker_active_bindings(m)
  expect_that(obj, is_a("active_bindings_manager"))

  ## This is not actually doing anything:
  ## NOTE: It might be a better test to run this with a fresh
  ## manager.
  maker_set_active_bindings(m, "target", obj)
  ## Replacing bindings is fine:
  expect_that(maker_set_active_bindings(m, "target", obj),
              not(throws_error()))

  expect_that(ls(obj$envir), equals("processed"))
  expect_that(d <- obj$envir$processed,
              shows_message("BUILD"))
  expect_that(d, is_a("data.frame"))
  expect_that(d <- obj$envir$processed,
              not(shows_message("BUILD")))
  expect_that(d, is_a("data.frame"))

  expect_that(obj$envir$processed <- 1,
              throws_error("read-only"))
  expect_that(bindingIsActive(quote(processed), obj$envir),
              is_true())
  ## Can be is_active_binding if my PR to pryr goes through:
  expect_that(bindingIsActive(quote(d), environment()),
              is_false())

  expect_that(obj$bindings$target, equals("processed"))
  expect_that("processed" %in% filter_active_bindings(ls(obj$envir), obj$envir),
              is_true())

  obj2 <- active_bindings_manager$new(new.env(), names(obj$bindings))
  obj2$envir$processed <- 1
  expect_that(maker_set_active_bindings(m, "target", obj2),
              throws_error("Bindngs would overwrite normal variables"))
  expect_that(bindingIsActive("processed", obj2$envir),
              is_false())
  expect_that(maker_set_active_bindings(m, "target", obj2, force=TRUE),
              not(throws_error()))
  expect_that(bindingIsActive("processed", obj2$envir), is_true())
})

test_that("Delete active bindings", {
  cleanup()
  m <- maker::maker(envir=new.env())
  obj <- maker_active_bindings(m)
  maker_set_active_bindings(m, "target", obj)

  del <- maker_delete_active_bindings(m, "target", obj)
  expect_that(del, equals("processed"))
  expect_that(ls(obj$envir), equals(character(0)))
})

test_that("Resolve active bindings", {
  ## THIS IS WHERE I AM UP TO.
  cleanup()
  m <- maker::maker(envir=new.env())
  obj <- maker_active_bindings(m)

  maker_set_active_bindings(m, "target", obj)

  expect_that(res <- maker_resolve_active_bindings(m, "target", obj),
              shows_message("SKIP"))
  expect_that(res, equals("processed"))
  expect_that(ls(obj$envir), equals("processed"))
  expect_that(bindingIsActive("processed", obj$envir), is_false())
  expect_that(obj$envir$processed, is_null())

  m <- maker::maker(envir=new.env())
  obj <- maker_active_bindings(m)
  maker_set_active_bindings(m, "target", obj)
  expect_that(res <- maker_resolve_active_bindings(m, "target", obj,
                                                   force=TRUE),
              shows_message("BUILD"))

  expect_that(res, equals("processed"))
  expect_that(bindingIsActive("processed", obj$envir), is_false())
  expect_that(obj$envir$processed, is_a("data.frame"))
})

test_that("Active sources", {
  cleanup()
  m <- maker::maker(envir=new.env())
  obj <- maker_active_bindings(m)
  obj2 <- maker_active_bindings_manager()
  m$load_sources()

  maker_set_active_bindings(m, "source", obj2)
  expect_that(ls(obj2$envir), equals(ls(m$store$env$env)))

  expect_that(all(ls(m$store$env$env) %in% ls(obj$envir)), is_true())

  expect_that(obj$envir$process_data,
              is_identical_to(m$store$env$env$process_data))
  expect_that(obj$bindings$source, equals(ls(obj2$envir)))

  ## Then, change a function:
  m$store$env$env$process_data <- identity
  expect_that(obj$envir$process_data, is_identical_to(identity))
  expect_that(obj2$envir$process_data, is_identical_to(identity))

  expect_that(obj$envir$process_data <- identity,
              throws_error("read-only"))
})

test_that("Global mode", {
  cleanup()
  e <- new.env()
  m <- maker::maker(envir=e)
  obj <- maker_active_bindings(m)

  expect_that(exists("processed", e), is_true())
  expect_that(exists("download_data", e), is_false())
  m$load_sources()
  expect_that(exists("processed", e), is_true())
  expect_that(exists("download_data", e), is_true())
  expect_that(bindingIsActive("processed", e), is_true())
  expect_that(bindingIsActive("download_data", e), is_true())

  maker_delete_active_bindings(m, "target", obj)
  maker_delete_active_bindings(m, "source", obj)
  expect_that(exists("processed", e), is_false())
  expect_that(exists("download_data", e), is_false())
})

if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-remake.R")
}

context("Active binding functions")

test_that("code_dependencies skips active bindings", {
  lava <- function(...) stop("I am lava, don't touch", call.=FALSE)
  makeActiveBinding("foo", lava, .GlobalEnv)
  on.exit(rm(list="foo", .GlobalEnv))
  expect_that(foo, throws_error("I am lava"))
  expect_that(foo <<- 1, throws_error("I am lava"))
  f <- function(foo) foo + 1
  nodeps <- list(functions=character(0), packages=character(0))
  expect_that(code_dependencies(f, FALSE), equals(nodeps))

  ## Arguments skip through:
  bar <- function() 1
  g <- function(bar) bar + 1
  expect_that(code_dependencies(g, FALSE), equals(nodeps))
})

test_that("Create active bindings", {
  cleanup()
  m <- remake::remake(envir=new.env())
  obj <- remake_active_bindings_manager()
  expect_that(obj, is_a("active_bindings_manager"))

  ## This is not actually doing anything:
  ## NOTE: It might be a better test to run this with a fresh
  ## manager.
  remake_set_active_bindings(m, "target", obj)
  ## Replacing bindings is fine:
  expect_that(remake_set_active_bindings(m, "target", obj),
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
  expect_that("processed" %in%
              filter_active_bindings(ls(obj$envir), obj$envir),
              is_true())

  obj <- remake_active_bindings_manager()
  obj$envir$processed <- 1
  expect_that(remake_set_active_bindings(m, "target", obj),
              throws_error("Bindngs would overwrite normal variables"))
  expect_that(bindingIsActive("processed", obj$envir),
              is_false())
  expect_that(remake_set_active_bindings(m, "target", obj, force=TRUE),
              not(throws_error()))
  expect_that(bindingIsActive("processed", obj$envir), is_true())
})

test_that("Delete active bindings", {
  cleanup()
  m <- remake::remake()
  obj <- remake_active_bindings_manager()
  remake_set_active_bindings(m, "target", obj)

  del <- remake_delete_active_bindings(m, "target", obj)
  expect_that(del, equals("processed"))
  expect_that(ls(obj$envir), equals(character(0)))
})

test_that("Resolve active bindings", {
  cleanup()
  m <- remake::remake()
  obj <- remake_active_bindings_manager()

  remake_set_active_bindings(m, "target", obj)

  expect_that(res <- remake_resolve_active_bindings(m, "target", obj),
              shows_message("SKIP"))
  expect_that(res, equals("processed"))
  expect_that(ls(obj$envir), equals("processed"))
  expect_that(bindingIsActive("processed", obj$envir), is_false())
  expect_that(obj$envir$processed, is_null())

  m <- remake::remake(envir=new.env())
  obj <- remake_active_bindings(m)
  remake_set_active_bindings(m, "target", obj)
  expect_that(res <- remake_resolve_active_bindings(m, "target", obj,
                                                   force=TRUE),
              shows_message("BUILD"))

  expect_that(res, equals("processed"))
  expect_that(bindingIsActive("processed", obj$envir), is_false())
  expect_that(obj$envir$processed, is_a("data.frame"))
})

test_that("Active sources", {
  cleanup()
  m <- remake::remake()
  obj <- remake_active_bindings_manager()

  remake_set_active_bindings(m, "source", obj)
  expect_that(ls(obj$envir), equals(ls(m$store$env$env)))

  expect_that(all(ls(m$store$env$env) %in% ls(obj$envir)), is_true())

  expect_that(obj$envir$process_data,
              is_identical_to(m$store$env$env$process_data))
  expect_that(obj$bindings$source, equals(ls(obj$envir)))

  ## Then, change a function:
  m$store$env$env$process_data <- identity
  expect_that(obj$envir$process_data, is_identical_to(identity))

  expect_that(obj$envir$process_data <- identity,
              throws_error("read-only"))
})

test_that("Global mode", {
  cleanup()
  e <- new.env()
  m <- remake::remake(envir=e)
  obj <- remake_active_bindings(m)

  expect_that(exists("processed", e), is_true())
  expect_that(exists("download_data", e), is_true())

  expect_that(bindingIsActive("processed", e), is_true())
  expect_that(bindingIsActive("download_data", e), is_true())

  remake_delete_active_bindings(m, "target", obj)
  remake_delete_active_bindings(m, "source", obj)
  expect_that(exists("processed", e), is_false())
  expect_that(exists("download_data", e), is_false())
})

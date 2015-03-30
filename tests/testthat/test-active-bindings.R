context("Active binding functions")

test_that("code_dependencies skips active bindings", {
  lava <- function(...) stop("I am lava, don't touch", call.=FALSE)
  makeActiveBinding("foo", lava, .GlobalEnv)
  on.exit(rm(list="foo", envir=.GlobalEnv))
  expect_that(foo, throws_error("I am lava"))
  expect_that(foo <<- 1, throws_error("I am lava"))
  e <- new.env(parent=.GlobalEnv)
  e$f <- function(foo) foo + 1
  environment(e$f) <- e

  nodeps <- list(functions=character(0), packages=character(0))
  expect_that(code_dependencies(e$f, FALSE), equals(nodeps))

  ## Arguments skip through:
  e$bar <- function() 1
  environment(e$bar) <- e
  e$g <- function(bar) bar + 1
  environment(e$g) <- e
  expect_that(code_dependencies(e$g, FALSE), equals(nodeps))
})

test_that("code_dependencies regression test", {
  e <- new.env(parent=.GlobalEnv)
  sys.source("bindings.R", e)
  expect_that(code_dependencies(e$f)$functions, equals("g"))

  lava <- function(...) stop("I am lava, don't touch", call.=FALSE)
  makeActiveBinding("g", lava, .GlobalEnv)
  on.exit(rm(list="g", envir=.GlobalEnv))
  expect_that(code_dependencies(e$f)$functions, equals("g"))
})

test_that("global binding manager", {
  expect_that(global_active_bindings, is_a("binding_manager"))
  expect_that(global_active_bindings$envir,
              is_identical_to(.GlobalEnv))
  expect_that(global_active_bindings$files, equals(character(0)))
  expect_that(global_active_bindings$bindings, equals(character(0)))
  expect_that(global_active_bindings$type, equals(character(0)))
  expect_that(global_active_bindings$file, equals(character(0)))

  expect_that(global_active_bindings$clear(),
              not(throws_error()))
})

test_that("low level", {
  cleanup()
  m <- remake()
  expect_that(global_active_bindings$files, equals(character(0)))

  ## Now, let's set the active bindings.
  global_active_bindings$set_bindings("source", m)
  expect_that(is_active_binding("download_data"), is_true())
  tmp <- download_data
  expect_that(tmp, is_identical_to(m$store$env$env$download_data))

  expect_that(global_active_bindings$files, equals("remake.yml"))
  expect_that(global_active_bindings$bindings,
              equals(ls(m$store$env$env)))
  n <- length(global_active_bindings$bindings)
  expect_that(global_active_bindings$file,
              equals(rep_len("remake.yml", n)))
  expect_that(global_active_bindings$type,
              equals(rep_len("source", n)))

  ## Add the targets:
  global_active_bindings$set_bindings("target", m)
  expect_that(global_active_bindings$bindings,
              equals(c(ls(m$store$env$env), "processed")))
  expect_that(global_active_bindings$file,
              equals(rep_len("remake.yml", n + 1L)))
  expect_that(global_active_bindings$type,
              equals(c(rep_len("source", n), "target")))

  expect_that(is_active_binding("processed"), is_true())
  expect_that(d <- processed,
              shows_message("BUILD"))

  expect_that(d, is_a("data.frame"))
  expect_that(d <- processed, not(shows_message()))
  expect_that(d, is_a("data.frame"))

  expect_that(processed <<- 1, throws_error("read-only"))

  ## Delete the active bindings:
  global_active_bindings$delete_bindings("remake.yml")
  expect_that(exists("processed"), is_false())
  expect_that(exists("download_data"), is_false())

  expect_that(global_active_bindings$file, equals(character(0)))
  expect_that(global_active_bindings$type, equals(character(0)))
  expect_that(global_active_bindings$files, equals(character(0)))
  expect_that(global_active_bindings$bindings, equals(character(0)))
})

test_that("High level", {
  cleanup()

  create_bindings()
  expect_that(exists("processed"), is_true())
  expect_that(is_active_binding("processed"), is_true())

  expect_that(d <- processed,
              shows_message("BUILD"))

  expect_that(d, is_a("data.frame"))
  expect_that(d <- processed, not(shows_message()))
  expect_that(d, is_a("data.frame"))

  expect_that(processed <<- 1, throws_error("read-only"))

  delete_bindings()
  expect_that(exists("processed"), is_false())
})

test_that("Source changes trigger rebuild on variable access", {
  cleanup()
  filename_code <- "remake_active.R"
  code <- "foo <- function() 2"
  writeLines(code, filename_code)

  create_bindings("remake_active.yml")

  expect_that(exists("obj", .GlobalEnv), is_true())
  expect_that(is_active_binding("obj"), is_true())

  expect_that(x <- obj, shows_message("[ BUILD ] obj"))
  expect_that(x <- obj, not(shows_message()))

  code <- paste0(code, " * 2")
  writeLines(code, filename_code)
  expect_that(x <- obj, shows_message("loading sources"))
  expect_that(x <- obj, not(shows_message()))

  code <- paste0(code, " * 2")
  writeLines(code, filename_code)
  expect_that(x <- obj, shows_message("[ BUILD ] obj"))
  expect_that(x <- obj, not(shows_message()))
})

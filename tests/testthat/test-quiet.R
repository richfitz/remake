context("Quiet")

test_that("Quieten targets", {
  cleanup()
  m <- remake("quiet.yml")
  store <- m$store

  msg <- "make some noise"

  t <- m$targets[["noisy_message"]]
  expect_false(t$quiet)
  expect_message(target_run(t, store),              msg)
  expect_message(target_run(t, store, quiet=TRUE),  NA)
  expect_message(target_run(t, store, quiet=FALSE), msg)

  t <- m$targets[["noisy_cat"]]
  expect_false(t$quiet)
  expect_output(target_run(t, store),              msg)
  expect_output(target_run(t, store, quiet=TRUE),  NA)
  expect_output(target_run(t, store, quiet=FALSE), msg)

  t <- m$targets[["noisy_warning"]]
  expect_false(t$quiet)
  expect_warning(target_run(t, store),              msg)
  expect_warning(target_run(t, store, quiet=TRUE),  msg)
  expect_warning(target_run(t, store, quiet=FALSE), msg)

  t <- m$targets[["noisy_error"]]
  expect_false(t$quiet)
  expect_error(target_run(t, store),              msg)
  expect_error(target_run(t, store, quiet=TRUE),  msg)
  expect_error(target_run(t, store, quiet=FALSE), msg)
})

test_that("Quiet targets", {
  cleanup()
  m <- remake("quiet.yml")
  store <- m$store

  msg <- "make some noise"

  t <- m$targets[["quiet_message"]]
  expect_true(t$quiet)
  expect_message(target_run(t, store),              NA)
  expect_message(target_run(t, store, quiet=TRUE),  NA)
  expect_message(target_run(t, store, quiet=FALSE), msg)

  t <- m$targets[["quiet_cat"]]
  expect_true(t$quiet)
  expect_output(target_run(t, store),              NA)
  expect_output(target_run(t, store, quiet=TRUE),  NA)
  expect_output(target_run(t, store, quiet=FALSE), msg)

  t <- m$targets[["quiet_warning"]]
  expect_true(t$quiet)
  expect_warning(target_run(t, store),              msg)
  expect_warning(target_run(t, store, quiet=TRUE),  msg)
  expect_warning(target_run(t, store, quiet=FALSE), msg)

  t <- m$targets[["quiet_error"]]
  expect_true(t$quiet)
  expect_error(target_run(t, store),              msg)
  expect_error(target_run(t, store, quiet=TRUE),  msg)
  expect_error(target_run(t, store, quiet=FALSE), msg)
})

test_that("From remake", {
  cleanup()
  msg <- "make some noise"
  m <- remake("quiet.yml", verbose=FALSE)
  expect_message(remake_make(m, "noisy_message"), msg)
  expect_output(remake_make(m, "noisy_cat"), msg)

  expect_message(remake_make(m, "quiet_message"), NA)
  expect_output(remake_make(m, "quiet_cat"),     NA)
})

test_that("Quiet remake", {
  msg <- "make some noise"
  cleanup()

  ## Next create a remake instance that suppresses output:
  m <- remake("quiet.yml", verbose=remake_verbose(FALSE, target=FALSE))

  expect_message(remake_make(m, "noisy_message"), NA)
  expect_output(remake_make(m, "noisy_cat"), NA)
  expect_message(remake_make(m, "quiet_message"), NA)
  expect_output(remake_make(m, "quiet_cat"),     NA)
})

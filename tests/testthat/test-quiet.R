context("Quiet")

test_that("Quieten targets", {
  cleanup()
  m <- remake("quiet.yml")
  store <- m$store

  msg <- "make some noise"

  t <- m$targets[["noisy_message"]]
  expect_that(t$quiet, is_false())
  expect_that(target_run(t, store),              shows_message(msg))
  expect_that(target_run(t, store, quiet=TRUE),  not(shows_message()))
  expect_that(target_run(t, store, quiet=FALSE), shows_message(msg))

  t <- m$targets[["noisy_cat"]]
  expect_that(t$quiet, is_false())
  expect_that(target_run(t, store),              prints_text(msg))
  expect_that(target_run(t, store, quiet=TRUE),  not(prints_text(msg)))
  expect_that(target_run(t, store, quiet=FALSE), prints_text(msg))

  t <- m$targets[["noisy_warning"]]
  expect_that(t$quiet, is_false())
  expect_that(target_run(t, store),              gives_warning(msg))
  expect_that(target_run(t, store, quiet=TRUE),  gives_warning(msg))
  expect_that(target_run(t, store, quiet=FALSE), gives_warning(msg))

  t <- m$targets[["noisy_error"]]
  expect_that(t$quiet, is_false())
  expect_that(target_run(t, store),              throws_error(msg))
  expect_that(target_run(t, store, quiet=TRUE),  throws_error(msg))
  expect_that(target_run(t, store, quiet=FALSE), throws_error(msg))
})

test_that("Quiet targets", {
  cleanup()
  m <- remake("quiet.yml")
  store <- m$store

  msg <- "make some noise"

  t <- m$targets[["quiet_message"]]
  expect_that(t$quiet, is_true())
  expect_that(target_run(t, store),              not(shows_message()))
  expect_that(target_run(t, store, quiet=TRUE),  not(shows_message()))
  expect_that(target_run(t, store, quiet=FALSE),     shows_message())

  t <- m$targets[["quiet_cat"]]
  expect_that(t$quiet, is_true())
  expect_that(target_run(t, store),              not(prints_text(msg)))
  expect_that(target_run(t, store, quiet=TRUE),  not(prints_text(msg)))
  expect_that(target_run(t, store, quiet=FALSE),     prints_text(msg))

  t <- m$targets[["quiet_warning"]]
  expect_that(t$quiet, is_true())
  expect_that(target_run(t, store),              gives_warning(msg))
  expect_that(target_run(t, store, quiet=TRUE),  gives_warning(msg))
  expect_that(target_run(t, store, quiet=FALSE), gives_warning(msg))

  t <- m$targets[["quiet_error"]]
  expect_that(t$quiet, is_true())
  expect_that(target_run(t, store),              throws_error(msg))
  expect_that(target_run(t, store, quiet=TRUE),  throws_error(msg))
  expect_that(target_run(t, store, quiet=FALSE), throws_error(msg))
})

test_that("From remake", {
  cleanup()
  msg <- "make some noise"
  m <- remake("quiet.yml", verbose=FALSE)
  expect_that(remake_make(m, "noisy_message"), shows_message(msg))
  expect_that(remake_make(m, "noisy_cat"), prints_text(msg))

  expect_that(remake_make(m, "quiet_message"), not(shows_message(msg)))
  expect_that(remake_make(m, "quiet_cat"),     not(prints_text(msg)))
})

test_that("Quiet remake", {
  msg <- "make some noise"
  cleanup()

  ## Next create a remake instance that suppresses output:
  m <- remake("quiet.yml", verbose=remake_verbose(FALSE, target=FALSE))

  expect_that(remake_make(m, "noisy_message"), not(shows_message(msg)))
  expect_that(remake_make(m, "noisy_cat"), not(prints_text(msg)))
  expect_that(remake_make(m, "quiet_message"), not(shows_message(msg)))
  expect_that(remake_make(m, "quiet_cat"),     not(prints_text(msg)))
})

test_that("Quiet chain", {
  skip("Pending rewrite, apparently")
  cleanup()
  m <- remake("quiet.yml", verbose=FALSE)

  msg <- "make some noise"
  msg2 <- "make some more noise"
  msg_chain <- paste(msg, msg2, sep="\n")
  msg_chain <- c(msg, msg2)

  ## Shows both messages:
  remake_remove_target(m, "noisy_chain")
  expect_that(remake_make(m, "noisy_chain"), shows_message(msg))
  remake_remove_target(m, "noisy_chain")
  expect_that(remake_make(m, "noisy_chain"), shows_message(msg2))
  remake_remove_target(m, "noisy_chain")
  expect_that(remake_make(m, "noisy_chain", quiet_target=TRUE),
              not(shows_message()))
  remake_remove_target(m, "noisy_chain")
  expect_that(remake_make(m, "noisy_chain", quiet_target=FALSE),
              shows_message(msg))
  remake_remove_target(m, "noisy_chain")
  expect_that(remake_make(m, "noisy_chain", quiet_target=FALSE),
              shows_message(msg2))

  ## Shows no message
  remake_remove_target(m, "quiet_chain")
  expect_that(remake_make(m, "quiet_chain"), not(shows_message()))
  remake_remove_target(m, "quiet_chain")
  expect_that(remake_make(m, "quiet_chain", quiet_target=FALSE),
              not(shows_message()))
  remake_remove_target(m, "quiet_chain")
  expect_that(remake_make(m, "quiet_chain", quiet_target=TRUE),
              not(shows_message()))
})

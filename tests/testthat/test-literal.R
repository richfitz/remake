context("Literals")

test_that("literals", {
  cleanup()
  str <- "my_constant <- pi"
  writeLines(str, "code_literal.R")

  m <- remake("remake_literal.yml")

  expect_that(m$targets$data1$depends,
              is_identical_to(character(0)))
  res <- remake_make(m, "data1")
  expect_that(res, is_identical_to(list(TRUE)))

  db <- m$store$db$get("data1")
  expect_that(db$fixed, equals(hash_object(res)))
  expect_that(db$depends, equals(empty_named_list()))
  expect_that(db$code$functions, equals(empty_named_list()))

  expect_that(remake_is_current(m, "data1"), is_true())

  res <- remake_make(m, "data2")
  expect_that(res, is_identical_to(list(pi)))

  db <- m$store$db$get("data2")
  expect_that(db$fixed, equals(hash_object(list(pi))))
  expect_that(db$fixed, equals(hash_object(res)))
  expect_that(db$depends, equals(empty_named_list()))
  expect_that(db$code$functions, equals(empty_named_list()))

  res <- remake_make(m, "data3")
  expect_that(res, is_identical_to(list(pi)))

  expect_that(db$fixed, equals(hash_object(list(pi))))
  expect_that(db$fixed, equals(hash_object(res)))
  expect_that(db$depends, equals(empty_named_list()))
  expect_that(db$code$functions, equals(empty_named_list()))

  ## Getting a bit more silly down here:
  res <- remake_make(m, "data4")
  expect_that(res, is_identical_to(list("pi")))

  res <- remake_make(m, "data5")
  expect_that(res, is_identical_to(list("my_constant", pi)))

  ## Rewrite the code and check that it forces a rebuild as the
  ## *value* of the constant changes.
  writeLines(paste(str, "* 2"), "code_literal.R")

  ## This doesn't trigger without the explicit refresh, which I don't
  ## want to be doing.
  res <- make("data3", remake_file="remake_literal.yml")
  expect_that(res, is_identical_to(list(2 * pi)))

  db <- m$store$db$get("data3")
  expect_that(db$fixed, equals(hash_object(list(2 * pi))))
  expect_that(db$fixed, equals(hash_object(res)))
  expect_that(db$depends, equals(empty_named_list()))
  expect_that(db$code$functions, equals(empty_named_list()))

  expect_that(res <- make("data3", remake_file="remake_literal.yml"),
              not(shows_message("BUILD")))
})

test_that("Clash", {
  expect_that(remake("remake_literal_clash.yml"),
              throws_error("target/literal clash"))
  expect_that(remake("remake_literal_clash.yml"),
              throws_error("data3: my_constant"))
  expect_that(remake("remake_literal_clash.yml"),
              throws_error("data5: my_constant"))
  expect_that(remake("remake_literal_clash.yml"),
              throws_error("data6: my_constant"))
})

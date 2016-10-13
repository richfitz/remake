context("Literals")

test_that("literals", {
  cleanup()
  str <- "my_constant <- pi"
  writeLines(str, "code_literal.R")

  m <- remake("remake_literal.yml")

  expect_identical(m$targets$data1$depends_name, character(0))
  res <- remake_make(m, "data1")
  expect_identical(res, list(TRUE))

  db <- m$store$db$get("data1")
  expect_equal(db$fixed, hash_object(res))
  expect_equal(db$depends, empty_named_list())
  expect_equal(db$code$functions, empty_named_list())

  expect_true(remake_is_current(m, "data1"))

  res <- remake_make(m, "data2")
  expect_identical(res, list(pi))

  db <- m$store$db$get("data2")
  expect_equal(db$fixed, hash_object(list(pi)))
  expect_equal(db$fixed, hash_object(res))
  expect_equal(db$depends, empty_named_list())
  expect_equal(db$code$functions, empty_named_list())

  res <- remake_make(m, "data3")
  expect_identical(res, list(pi))

  expect_equal(db$fixed, hash_object(list(pi)))
  expect_equal(db$fixed, hash_object(res))
  expect_equal(db$depends, empty_named_list())
  expect_equal(db$code$functions, empty_named_list())

  ## Getting a bit more silly down here:
  res <- remake_make(m, "data4")
  expect_identical(res, list("pi"))

  res <- remake_make(m, "data5")
  expect_identical(res, list("my_constant", pi))

  ## Rewrite the code and check that it forces a rebuild as the
  ## *value* of the constant changes.
  writeLines(paste(str, "* 2"), "code_literal.R")

  ## This doesn't trigger without the explicit refresh, which I don't
  ## want to be doing.
  res <- make("data3", remake_file="remake_literal.yml")
  expect_identical(res, list(2 * pi))

  db <- m$store$db$get("data3")
  expect_equal(db$fixed, hash_object(list(2 * pi)))
  expect_equal(db$fixed, hash_object(res))
  expect_equal(db$depends, empty_named_list())
  expect_equal(db$code$functions, empty_named_list())

  msg <- capture_messages(make("data3", remake_file="remake_literal.yml"))
  expect_false(any(grepl("BUILD", msg)))
})

test_that("Clash", {
  expect_error(remake("remake_literal_clash.yml"),
               "target/literal clash")
  expect_error(remake("remake_literal_clash.yml"),
               "data3: my_constant")
  expect_error(remake("remake_literal_clash.yml"),
               "data5: my_constant")
  expect_error(remake("remake_literal_clash.yml"),
               "data6: my_constant")
})

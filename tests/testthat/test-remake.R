if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-remake.R")
}

context("Loading remakefiles")

test_that("Recursive remakefiles", {
  expect_that(remake("recursive1.yml"),
              throws_error("Recursive include detected"))
  expect_that(remake("recursive2.yml"),
              throws_error("Recursive include detected"))
  expect_that(remake("recursive3.yml"),
              throws_error("Recursive include detected"))
})

test_that("Target/rule clash", {
  cleanup()
  expect_that(m <- remake("remake_rule_clash.yml"),
              gives_warning("Rule name clashes with target name"))
  m$make()
  expect_that(file.exists("plot.pdf"), is_true())
})

test_that("Quoting", {
  template <- '
sources:
  - code.R

targets:
  data.csv:
    command: download_data(%s)
    cleanup_level: purge

  processed:
    command: process_data(%s)

  plot.pdf:
    command: myplot(%s)
    plot: true
'
  ## First the OK version:
  filename <- "tmp_quoting.yml"
  on.exit(file.remove(filename))

  writeLines(sprintf(template, 'target_name', '"data.csv"', 'processed'),
             filename)
  expect_that(remake(filename), not(throws_error()))

  writeLines(sprintf(template, '"data.csv"', '"data.csv"', 'processed'),
             filename)
  expect_that(remake(filename), not(throws_error()))

  ## Then different errors:
  ## The first two are *useless* errors
  writeLines(sprintf(template, '"target_name"', '"data.csv"', 'processed'),
             filename)
  expect_that(remake(filename),
              throws_error("target_name must not be quoted"))

  writeLines(sprintf(template, 'data.csv', '"data.csv"', 'processed'),
             filename)
  expect_that(remake(filename),
              throws_error("target name must be quoted"))

  writeLines(sprintf(template, 'target_name', 'data.csv', 'processed'),
             filename)
  expect_that(remake(filename),
              throws_error("Incorrect quotation in target"))
  expect_that(remake(filename),
              throws_error("Should be quoted: data.csv"))

  writeLines(sprintf(template, 'target_name', '"data.csv"', '"processed"'),
             filename)
  expect_that(remake(filename),
              throws_error("Incorrect quotation in target"))
  expect_that(remake(filename),
              throws_error("Should not be quoted: processed"))
})

test_that("Verbosity", {
  default <- remake_verbose()
  expect_true(default$print_progress)
  expect_true(default$print_noop)
  expect_true(default$print_command)
  expect_true(default$print_command_abbreviate)
  expect_null(default$quiet_target)

  quiet <- remake_verbose(FALSE)
  expect_false(quiet$print_progress)
  expect_true(quiet$print_noop)
  expect_true(quiet$print_command)
  expect_true(quiet$print_command_abbreviate)
  expect_null(quiet$quiet_target)

  x <- remake_verbose(default)
  expect_identical(x, default)

  expect_error(remake_verbose(1), "verbose must be logical")
})

test_that("literals", {
  cleanup()
  str <- "my_constant <- pi"
  writeLines(str, "code_literal.R")
  
  m <- remake("remake_literal.yml")
  
  expect_that(m$targets$data1$depends,
              is_identical_to(empty_named_integer()))
  res <- m$make("data1")
  expect_that(res, is_identical_to(list(TRUE)))

  db <- m$store$db$get("data1")
  expect_that(db$fixed, equals(hash_object(res)))
  expect_that(db$depends, equals(empty_named_list()))
  expect_that(db$code$functions, equals(empty_named_list()))
  expect_that(db$code$packages, equals(list()))

  expect_that(is_current("data1", m), is_true())

  res <- m$make("data2")
  expect_that(res, is_identical_to(list(pi)))

  db <- m$store$db$get("data2")
  expect_that(db$fixed, equals(hash_object(list(pi))))
  expect_that(db$fixed, equals(hash_object(res)))
  expect_that(db$depends, equals(empty_named_list()))
  expect_that(db$code$functions, equals(empty_named_list()))
  expect_that(db$code$packages, equals(list()))
  
  res <- m$make("data3")
  expect_that(res, is_identical_to(list(pi)))
  
  expect_that(db$fixed, equals(hash_object(list(pi))))
  expect_that(db$fixed, equals(hash_object(res)))
  expect_that(db$depends, equals(empty_named_list()))
  expect_that(db$code$functions, equals(empty_named_list()))
  expect_that(db$code$packages, equals(list()))

  ## Getting a bit more silly down here:
  res <- m$make("data4")
  expect_that(res, is_identical_to(list("pi")))

  res <- m$make("data5")
  expect_that(res, is_identical_to(list("my_constant", pi)))

  res <- m$make("data6")
  expect_that(res, is_identical_to(list("my_constant", pi, list(TRUE))))

  ## Rewrite the code and check that it forces a rebuild as the
  ## *value* of the constant changes.
  writeLines(paste(str, "* 2"), "code_literal.R")
  res <- m$make("data3")
  
  expect_that(res, is_identical_to(list(2 * pi)))

  db <- m$store$db$get("data3")
  expect_that(db$fixed, equals(hash_object(list(2 * pi))))
  expect_that(db$fixed, equals(hash_object(res)))
  expect_that(db$depends, equals(empty_named_list()))
  expect_that(db$code$functions, equals(empty_named_list()))
  expect_that(db$code$packages, equals(list()))
})

test_that("Caching", {
  cleanup()
  expect_that(cache$fetch("remake.yml", TRUE, NULL),
              is_null())
  m <- remake("remake.yml")
  expect_that(cache$fetch("remake.yml", TRUE, NULL),
              equals(m))
  cleanup()
  expect_that(cache$fetch("remake.yml", TRUE, NULL),
              is_null())

  e <- new.env()
  expect_that(cache$fetch("remake.yml", TRUE, e), is_null())
  m2 <- remake(envir=e)
  expect_that(cache$fetch("remake.yml", TRUE, e), equals(m2))
})

test_that("Loading without sources", {
  cleanup()
  ## No caching here:
  expect_that(m <- .R6_remake$new(load_sources=FALSE),
              not(shows_message()))
  expect_that(m <- .R6_remake$new(load_sources=TRUE),
              shows_message("loading sources"))

  ## With caching:
  cleanup()
  expect_that(m <- remake2(load_sources=FALSE), not(shows_message()))
  expect_that(m$store$env,     is_a("managed_environment"))
  expect_that(m$store$env$env, is_null())
  ## This means that things *will* get cleared
  expect_that(m$store$env$is_current(), is_false())

  ## And again from the cache no message:
  expect_that(m <- remake2(load_sources=FALSE), not(shows_message()))

  ## Check again:
  m <- cache$fetch("remake.yml", TRUE, NULL)
  expect_that(m, is_a("remake"))
  expect_that(m$store$env$env, is_null())

  ## This does load the sources
  expect_that(m <- remake2(), shows_message("loading sources"))
  ## Sources already loaded
  expect_that(m <- remake2(), not(shows_message("loading sources")))
})

## Tests of low-level target validity.  Some of these will require
## considerable mocking up.  I'm not doing this via yaml, because that
## seems like a pain :)
if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Targets (low level)")

## The simplest target types:
test_that("Fake targets", {
  t <- make_target("a_fake_target", list(), "fake")

  expect_that(t, is_a("target_fake"))
  expect_that(t, is_a("target_base"))
  expect_that(t$type, equals("fake"))

  expect_that(t$name, equals("a_fake_target"))
  expect_that(t$depends, equals(list()))
  expect_that(t$rule, equals(NULL))
  expect_that(t$cleanup_level, equals("never"))
  expect_that(t$quiet, equals(FALSE))

  ## For this set of arguments we'd infer a fake target, too:
  t <- make_target("a_fake_target", list())
  expect_that(t$type, equals("fake"))

  deps <- letters[1:3]
  t <- make_target("a_fake_target", list(depends=deps))
  expect_that(t$type, equals("fake"))
  expect_that(t$depends, equals(as.list(deps)))
})

test_that("Fake targets (invalid)", {
  expect_that(make_target("fake", list(rule="foo"), type="fake"),
              throws_error("fake targets must have a null rule"))
  expect_that(make_target("fake", list(target_argument="foo"), type="fake"),
              throws_error("'target_argument' field invalid for"))
  expect_that(make_target("fake", list(quiet=TRUE), type="fake"),
              gives_warning("has no effect"))
  expect_that(make_target("fake", list(cleanup_level="tidy"), type="fake"),
              throws_error("Invalid options for fake: cleanup_level"))
  expect_that(make_target("fake", list(other_opt="tidy"), type="fake"),
              throws_error("Invalid options for fake: other_opt"))
})

test_that("Dependency parsing", {
  ## Empty:
  expect_that(from_yaml_map_list(yaml_load("[]")),
              equals(list()))

  ## No argument names:
  expect_that(from_yaml_map_list(yaml_load("[a, b, c]")),
              equals(list("a", "b", "c")))
  expect_that(from_yaml_map_list(yaml_load("- a\n- b\n- c")),
              equals(list("a", "b", "c")))

  expect_that(from_yaml_map_list(yaml_load("[A: a, b, c]")),
              equals(list(A="a", "b", "c")))
  expect_that(from_yaml_map_list(yaml_load("- A: a\n- b\n- c")),
              equals(list(A="a", "b", "c")))
})

test_that("Object target", {
  t <- make_target("real", list(rule="foo"))

  expect_that(t, is_a("target_object"))
  expect_that(t, is_a("target_base"))
  expect_that(t$type, equals("object"))

  expect_that(t$name, equals("real"))
  expect_that(t$depends, equals(list()))
  expect_that(t$rule, equals("foo"))
  expect_that(t$cleanup_level, equals("tidy"))
  expect_that(t$quiet, equals(FALSE))

  expect_that(t$run_fake(), equals("real <- foo()"))

  ## Using the command interface
  t <- make_target("real", list(command="foo()"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(list()))

  ## Passing options:
  t <- make_target("real", list(rule="foo", quiet=TRUE))
  expect_that(t$quiet, equals(TRUE))

  t <- make_target("real", list(rule="foo", cleanup_level="purge"))
  expect_that(t$cleanup_level, equals("purge"))

  ## With dependencies:
  t <- make_target("real", list(rule="foo", depends=list("a")))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals("a"))

  t <- make_target("real", list(command="foo(a)"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals("a"))

  t <- make_target("real", list(command="foo(a, b=c)"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(c("a", b="c")))
})

## TODO: These error messages are super inconsistent.
test_that("Object target (invalid)", {
  ## This is actually hard to achive:
  expect_that(make_target("real", list(), type="object"),
              throws_error("Must not have a NULL rule"))

  expect_that(make_target("real", list(rule="foo", target_argument=1)),
              throws_error("'target_argument' field invalid"))

  expect_that(make_target("real", list(rule=c("foo", "bar"))),
              throws_error("real: rule must be a scalar"))

  expect_that(make_target("real", list(rule="foo", quiet="quiet")),
              throws_error("real: quiet must be logical"))
  expect_that(make_target("real", list(rule="foo", quiet=c(TRUE, TRUE))),
              throws_error("real: quiet must be a scalar"))

  expect_that(make_target("real", list(rule="foo", cleanup_level="purge2")),
              throws_error("real: cleanup_level must be one"))

  expect_that(make_target("real", list(rule="foo", other_opt="tidy")),
              throws_error("Invalid options for real: other_opt"))
})

test_that("File targets", {
  t <- make_target("foo.csv", list(rule="foo"))
  expect_that(t$type, equals("file"))
  expect_that(t, is_a("target_file"))
  expect_that(t, is_a("target_base"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(list()))
  expect_that(t$target_argument, is_null())
  expect_that(t$run_fake(), equals("foo()"))

  deps <- from_yaml_map_list(yaml_load("[a, b, c]"))
  t <- make_target("foo.csv", list(rule="foo", depends=deps))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(c("a", "b", "c")))
  expect_that(t$target_argument, is_null())

  deps <- from_yaml_map_list(yaml_load("[a, b, C: c]"))
  t <- make_target("foo.csv", list(rule="foo", depends=deps))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(c("a", "b", C="c")))
  expect_that(t$target_argument, is_null())

  t <- make_target("foo.csv", list(rule="foo", depends=deps,
                                   target_argument=1))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(c("a", "b", C="c")))
  expect_that(t$target_argument, equals(1))

  t <- make_target("foo.csv", list(rule="foo", depends=deps,
                                   target_argument="name"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(c("a", "b", C="c")))
  expect_that(t$target_argument, equals("name"))
})

test_that("Implicit file targets", {
  t <- make_target("code.R", NULL, "file")
  expect_that(t$name, equals("code.R"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, is_null())
  expect_that(t$depends, equals(list()))
  expect_that(t$build(), throws_error("Can't build implicit targets"))
  expect_that(t$run(), is_null())
  expect_that(t$run_fake(), is_null())

  expect_that(t <- make_target("file.csv", NULL, "file"),
              gives_warning("Creating implicit target for nonexistant"))
  expect_that(t$name, equals("file.csv"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, is_null())
  ## TODO: empty depends should be character(0), or nonempty lists
  ## should be lists.
  expect_that(t$depends, equals(list()))
  expect_that(t$build(), throws_error("Can't build implicit targets"))
  expect_that(t$run(), is_null())
  expect_that(t$run_fake(), is_null())
})

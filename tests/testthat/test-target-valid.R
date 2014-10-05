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
  expect_that(t$check, equals("all"))

  ## For this set of arguments we'd infer a fake target, too:
  t <- make_target("a_fake_target", list())
  expect_that(t$type, equals("fake"))

  deps <- letters[1:3]
  t <- make_target("a_fake_target", list(depends=deps))
  expect_that(t$type, equals("fake"))
  expect_that(t$depends, equals(as.list(deps)))
})

test_that("Can't do much with fake targets", {
  t <- make_target("a_fake_target", list(), "fake")
  expect_that(t$get(), throws_error("Not something that can be"))
  expect_that(t$set(), throws_error("Not something that can be"))
  expect_that(t$set(1), throws_error("Not something that can be"))
  expect_that(t$del(), throws_error("Not something that can be"))
  expect_that(t$copy(), throws_error("Not something that can be"))
  expect_that(t$copy(tempdir()), throws_error("Not something that can be"))
})

test_that("Fake targets (invalid)", {
  expect_that(make_target("fake", list(rule="foo"), type="fake"),
              throws_error("fake targets must have a NULL rule"))
  expect_that(make_target("fake", list(target_argument="foo"), type="fake"),
              throws_error("'target_argument' field invalid for"))
  expect_that(make_target("fake", list(quiet=TRUE), type="fake"),
              gives_warning("has no effect"))
  expect_that(make_target("fake", list(check="exists"), type="fake"),
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
  expect_that(t$check, equals("all"))

  expect_that(t$run_fake(), equals("real <- foo()"))

  ## Using the command interface
  t <- make_target("real", list(command="foo()"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(list()))

  ## Passing options:
  t <- make_target("real", list(rule="foo", quiet=TRUE))
  expect_that(t$quiet, equals(TRUE))

  t <- make_target("real", list(rule="foo", check="code"))
  expect_that(t$check, equals("code"))

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

test_that("knitr", {
  t <- make_target("file.md", list(), "knitr")
  expect_that(t, is_a("target_knitr"))
  ## TODO: Not sure that this is correct (see elsewhere for plot though)
  expect_that(t$type, equals("file"))

  ## Inferred correct input:
  expect_that(t$knitr$input, equals("file.Rmd"))

  ## Allow knitr in opts:
  t <- make_target("file.md", list(knitr=TRUE))
  expect_that(t, is_a("target_knitr"))

  ## Only valid option is "input":
  t <- make_target("file.md", list(knitr=list(input="foo.Rmd")))
  expect_that(t$knitr$input, equals("foo.Rmd"))

  ## Quiet by default:
  expect_that(t$quiet, is_true())
  expect_that(make_target("file.md", list(quiet=TRUE), "knitr")$quiet,
              is_true())
  expect_that(make_target("file.md", list(quiet=FALSE), "knitr")$quiet,
              is_false())

  ## This might change
  expect_that(t$rule, equals(".__knitr__"))
})

test_that("knitr (invalid)", {
  expect_that(make_target("file.xmd", list(knitr=TRUE)),
              throws_error("Target must end in .md"))

  expect_that(make_target("file.md", list(rule="fn"), "knitr"),
              throws_error("knitr targets must have a NULL rule"))

  expect_that(make_target("file.md", list(quiet="yes please"), "knitr")$quiet,
              throws_error("file.md: quiet must be logical"))

  expect_that(make_target("file.md", list(unknown="opt"), "knitr"),
              throws_error("Invalid options for file.md"))
})

## Things that need activation:
test_that("get/set/copy/del object targets", {
  cleanup()
  m <- maker$new("maker.yml")
  m$make("processed")
  t <- m$get_target("processed")
  expect_that(t$get(), is_a("data.frame"))
  dep <- t$dependency_status()

  path <- tempfile()
  dir.create(path)
  t$copy(path)

  expect_that(dir(file.path(path, "objects")),
              equals(c("processed", "processed__hash")))
  expect_that(readRDS(file.path(path, "objects", "processed")),
               equals(t$get()))
  expect_that(readLines(file.path(path, "objects", "processed__hash")),
               equals(digest::digest(t$get())))

  name <- paste0(digest::digest(t$name), ".rds")
  expect_that(dir(file.path(path, "db")), equals(name))
  expect_that(readRDS(file.path(path, "db", name)), equals(dep))
  unlink(path, recursive=TRUE)

  ## Set this to rubbish values:
  t$set("foo")
  expect_that(t$get(), equals("foo"))

  t$del()
  expect_that(m$store$contains("processed", "object"), is_false())
  expect_that(t$del(),
              throws_error("processed not found in object store"))
  expect_that(t$del(missing_ok=TRUE), is_false())

  path <- tempfile()
  dir.create(path)
  expect_that(t$copy(path),
              throws_error("processed not found in object store"))

  expect_that(t$copy(path, missing_ok=TRUE), is_false())
  unlink(path, recursive=TRUE)
})

## Things that need activation:
test_that("get/set/copy/del file targets", {
  cleanup()
  m <- maker$new("maker.yml")
  m$make("plot.pdf")
  t <- m$get_target("plot.pdf")
  expect_that(t$get(), equals("plot.pdf"))
  dep <- t$dependency_status()

  path <- tempfile()
  dir.create(path)
  t$copy(path)

  md5 <- function(f) unname(tools::md5sum(f))
  expect_that(dir(file.path(path, "files")), equals("plot.pdf"))
  expect_that(md5(file.path(path, "files", "plot.pdf")),
              equals(md5("plot.pdf")))

  name <- paste0(digest::digest(t$name), ".rds")
  expect_that(dir(file.path(path, "db")), equals(name))
  expect_that(readRDS(file.path(path, "db", name)), equals(dep))
  unlink(path, recursive=TRUE)

  t$del()
  expect_that(m$store$contains("plot.pdf", "file"), is_false())
  expect_that(file.exists("plot.pdf"), is_false())

  expect_that(t$del(),
              throws_error("plot.pdf not found in file store"))
  expect_that(t$del(missing_ok=TRUE), is_false())

  path <- tempfile()
  dir.create(path)
  expect_that(t$copy(path),
              throws_error("plot.pdf not found in file store"))

  expect_that(t$copy(path, missing_ok=TRUE), is_false())
  unlink(path, recursive=TRUE)
})

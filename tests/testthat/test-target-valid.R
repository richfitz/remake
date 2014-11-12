## Tests of low-level target validity.  Some of these will require
## considerable mocking up.  I'm not doing this via yaml, because that
## seems like a pain :)
if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Targets (low level)")

test_that("reserved names", {
  reserved <- c("install_packages", "gitignore", "target_name", ".")
  expect_that(sort(reserved), equals(sort(target_reserved_names())))
  for (i in reserved) {
    expect_that(make_target(i), throws_error("Target name .+ is reserved"))
  }
})

## The simplest target types:
test_that("Fake targets", {
  t <- make_target("a_fake_target", list(type="fake"))

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
  t <- make_target("a_fake_target", list(type="fake"))
  expect_that(t$get(), throws_error("Not something that can be"))
  expect_that(t$set(), throws_error("Not something that can be"))
  expect_that(t$set(1), throws_error("Not something that can be"))
  expect_that(t$del(), throws_error("Not something that can be"))
  expect_that(t$archive_export(), throws_error("Not something that can be"))
  expect_that(t$archive_export(tempdir()), throws_error("Not something that can be"))
})

test_that("Fake targets (invalid)", {
  expect_that(make_target("fake", list(command="foo()", type="fake")),
              throws_error("fake targets must have a NULL rule"))
  expect_that(make_target("fake", list(target_argument="foo", type="fake")),
              throws_error("Invalid keys: target_argument"))
  expect_that(make_target("fake", list(quiet=TRUE, type="fake")),
              gives_warning("has no effect"))
  expect_that(make_target("fake", list(check="exists", type="fake")),
              gives_warning("has no effect"))
  expect_that(make_target("fake", list(cleanup_level="tidy", type="fake")),
              throws_error("Invalid options for fake: cleanup_level"))
  expect_that(make_target("fake", list(other_opt="tidy", type="fake")),
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
  t <- make_target("real", list(command="foo()"))

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

  ## Passing options:
  t <- make_target("real", list(command="foo()", quiet=TRUE))
  expect_that(t$quiet, equals(TRUE))

  t <- make_target("real", list(command="foo()", check="code"))
  expect_that(t$check, equals("code"))

  t <- make_target("real", list(command="foo()", cleanup_level="purge"))
  expect_that(t$cleanup_level, equals("purge"))

  ## With dependencies:
  t <- make_target("real", list(command="foo()", depends=list("a")))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals("a"))

  t <- make_target("real", list(command="foo(a)"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals("a"))

  t <- make_target("real", list(command="foo(a, b=c)"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(c("a", b="c")))

  t <- make_target("code.R", list(command="foo()", type="object"))
  expect_that(t$type, equals("object"))
})

## TODO: These error messages are super inconsistent.
test_that("Object target (invalid)", {
  expect_that(make_target("foo", "bar"),
              throws_error("While processing target 'foo':"))
  expect_that(make_target("foo", "bar"),
              throws_error("target data must be named"))
  expect_that(make_target("foo", c(name="bar")),
              throws_error("target data must be a list"))

  ## This is actually hard to achive:
  expect_that(make_target("real", list(type="object")),
              throws_error("Must not have a NULL rule"))

  expect_that(make_target("real", list(command="foo()", target_argument=1)),
              throws_error("Invalid keys: target_argument"))
  expect_that(make_target("real", list(rule="foo")),
              throws_error("Invalid keys: rule"))

  expect_that(make_target("real", list(command="foo()", quiet="quiet")),
              throws_error("quiet must be logical"))
  expect_that(make_target("real", list(command="foo()", quiet=c(TRUE, TRUE))),
              throws_error("quiet must be a scalar"))

  expect_that(make_target("real", list(command="foo()",
                                       cleanup_level="purge2")),
              throws_error("cleanup_level must be one"))

  expect_that(make_target("real", list(command="foo()", check="exists2")),
              throws_error("check must be one"))

  expect_that(make_target("real", list(command="foo()", other_opt="tidy")),
              throws_error("Invalid options for real: other_opt"))
})

test_that("File targets", {
  t <- make_target("foo.csv", list(command="foo()"))
  expect_that(t$type, equals("file"))
  expect_that(t, is_a("target_file"))
  expect_that(t, is_a("target_base"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(list()))
  expect_that(t$target_argument, is_null())
  expect_that(t$run_fake(), equals("foo()"))

  t <- make_target("foo.csv", list(command="foo(a, b, c)"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(c("a", "b", "c")))
  expect_that(t$target_argument, is_null())

  t <- make_target("foo.csv", list(command="foo(a, b, C=c)"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(c("a", "b", C="c")))
  expect_that(t$target_argument, is_null())

  t <- make_target("foo.csv", list(command="foo(target_name, b, C=c)"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(c("b", C="c")))
  expect_that(t$target_argument, equals(1))

  t <- make_target("foo.csv", list(command="foo(name='foo.csv', b, C=c)"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends, equals(c("b", C="c")))
  expect_that(t$target_argument, equals("name"))

  t <- make_target("code.R", list())
  expect_that(t$type, equals("file"))
})

test_that("Implicit file targets", {
  t <- make_target("code.R", list(type="file"))
  expect_that(t$name, equals("code.R"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, is_null())
  expect_that(t$depends, equals(list()))
  expect_that(t$build(), throws_error("Can't build implicit targets"))
  expect_that(t$run(), is_null())
  expect_that(t$run_fake(), is_null())

  expect_that(t <- make_target("file.csv", list(type="file")),
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
  t <- make_target("file.md", list(type="knitr"))
  expect_that(t, is_a("target_knitr"))
  ## TODO: Not sure that this is correct (see elsewhere for plot though)
  expect_that(t$type, equals("file"))

  ## Inferred correct input:
  expect_that(t$knitr$input, equals("file.Rmd"))

  ## No options set
  expect_that(t$knitr$options, equals(list(error=FALSE)))

  ## Allow knitr in opts:
  t <- make_target("file.md", list(knitr=TRUE))
  expect_that(t, is_a("target_knitr"))

  ## Only valid option is "input":
  t <- make_target("file.md", list(knitr=list(input="foo.Rmd")))
  expect_that(t$knitr$input, equals("foo.Rmd"))

  ## Test auto_figure_prefix:
  t <- make_target("file.md", list(knitr=TRUE, auto_figure_prefix=TRUE))
  expect_that(t$knitr$options$fig.path, equals("figure/file__"))

  ## auto_figure_prefix works off the *target* name, not the Rmd name
  ## (when different)
  t <- make_target("file.md", list(knitr=list(input="other_file.Rmd"),
                                   auto_figure_prefix=TRUE))
  expect_that(t$knitr$options$fig.path, equals("figure/file__"))

  prefix <- "figures/file-"
  t <- make_target("file.md",
                   list(knitr=list(options=list(fig.path=prefix))))
  expect_that(t$knitr$options$fig.path, equals(prefix))

  ## Quiet by default:
  expect_that(t$quiet, is_true())
  expect_that(make_target("file.md", list(quiet=TRUE, type="knitr"))$quiet,
              is_true())
  expect_that(make_target("file.md", list(quiet=FALSE, type="knitr"))$quiet,
              is_false())

  ## This might change
  expect_that(t$rule, equals(".__knitr__"))
})

test_that("knitr (invalid)", {
  expect_that(make_target("file.xmd", list(knitr=TRUE)),
              throws_error("Target must end in .md"))

  expect_that(make_target("file.md", list(command="fn()", type="knitr")),
              throws_error("knitr targets must have a NULL rule"))

  expect_that(make_target("file.md", list(command="fn()", knitr=TRUE)),
              throws_error("knitr targets must have a NULL rule"))
  ## Chains fail because commands fail:
  expect_that(make_target("file.md",
                          list(command=c("foo()", "bar(.)"), knitr=TRUE)),
              throws_error("knitr targets must have a NULL rule"))

  expect_that(make_target("file.md", list(quiet="yes please",
                                          type="knitr"))$quiet,
              throws_error("quiet must be logical"))

  expect_that(make_target("file.md", list(unknown="opt", type="knitr")),
              throws_error("Invalid options for file.md"))

  expect_that(make_target("file.md", list(knitr=list(auto_figure_prefix=TRUE))),
              gives_warning("Unknown fields in knitr: auto_figure"))

  expect_that(make_target("file.md", list(knitr=list(auto_figure_prefix=TRUE))),
              gives_warning("Unknown fields in knitr: auto_figure"))

  dat <- list(knitr=list(options=list(fig.path="foo")),
              auto_figure_prefix=TRUE)
  expect_that(make_target("file.md", dat),
              gives_warning("Ignoring 'auto_figure_prefix'"))
})

## This section is weird and pretty much just a regression test.  I
## hope that this sort of functionality is not actually that useful.
test_that("cleanup", {
  cleanup()
  m <- maker$new("maker_cleanup_hook.yml")
  t <- m$get_target("clean")
  expect_that(length(t$depends), equals(2))
  expect_that(dependency_names(t$depends),
              equals(c("data.csv", "tidy")))

  expect_that(file.exists("data.csv"), is_false())
  expect_that(m$make("clean"),
              shows_message("running post-cleanup hook"))
  expect_that(file.exists("data.csv"), is_true())
  expect_that(m$make("purge"),
              shows_message("running post-cleanup hook"))
  expect_that(file.exists("data.csv"), is_false())

  cleanup()
  expect_that(maker$new("maker_cleanup_error.yml"),
              throws_error("Cleanup target commands must have no arguments"))
})

## Things that need activation:
test_that("get/set/archive/del object targets", {
  cleanup()
  m <- maker$new("maker.yml")
  m$make("processed")
  t <- m$get_target("processed")
  expect_that(t$get(), is_a("data.frame"))
  dep <- t$dependency_status()

  path <- tempfile()
  dir.create(path)
  t$archive_export(path)

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
  expect_that(t$archive_export(path),
              throws_error("processed not found in object store"))

  expect_that(t$archive_export(path, missing_ok=TRUE), is_false())
  unlink(path, recursive=TRUE)
})

## Things that need activation:
test_that("get/set/archive/del file targets", {
  cleanup()
  m <- maker$new("maker.yml")
  m$make("plot.pdf")
  t <- m$get_target("plot.pdf")
  expect_that(t$get(), equals("plot.pdf"))
  dep <- t$dependency_status()

  path <- tempfile()
  dir.create(path)
  t$archive_export(path)

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
  expect_that(t$archive_export(path),
              throws_error("plot.pdf not found in file store"))

  expect_that(t$archive_export(path, missing_ok=TRUE), is_false())
  unlink(path, recursive=TRUE)
})

test_that("Error messages", {
  expect_that(maker$new("maker_invalid.yml"),
              throws_error("While processing target 'all'"))
})

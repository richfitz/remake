## Tests of low-level target validity.  Some of these will require
## considerable mocking up.  I'm not doing this via yaml, because that
## seems like a pain :)
context("Targets (low level)")

test_that("reserved names", {
  reserved <- c("target_name", ".")
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
  expect_that(t$depends_name, equals(character(0)))
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
  expect_that(t$depends_name, equals(deps))

  ## Same as above, but with packages:
  expect_that(make_target("a_fake_target",
                          list(depends=deps, packages="pkg")),
              throws_error("fake targets may not load packages"))
})

test_that("Can't do much with fake targets", {
  t <- make_target("a_fake_target", list(type="fake"))
  expect_that(target_get(t), throws_error("Not something that can be"))
  expect_that(target_set(t), throws_error("Not something that can be"))
})

test_that("Fake targets (invalid)", {
  expect_that(make_target("fake", list(command="foo()", type="fake")),
              throws_error("fake targets must have a NULL rule"))
  expect_that(make_target("fake", list(target_argument="foo", type="fake")),
              throws_error("Invalid keys: target_argument"))
  expect_that(make_target("fake", list(cleanup_level="tidy", type="fake")),
              throws_error("Unknown fields in fake: cleanup_level"))
  expect_that(make_target("fake", list(other_opt="tidy", type="fake")),
              throws_error("Unknown fields in fake: other_opt"))
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
  expect_that(t$depends, equals(character(0)))
  expect_that(t$depends_name, equals(character(0)))
  expect_that(t$rule, equals("foo"))
  expect_that(t$cleanup_level, equals("tidy"))
  expect_that(t$quiet, equals(FALSE))
  expect_that(t$check, equals("all"))

  expect_that(target_run_fake(t), equals("real <- foo()"))

  ## Passing options:
  t <- make_target("real", list(command="foo()", quiet=TRUE))
  expect_that(t$quiet, equals(TRUE))

  t <- make_target("real", list(command="foo()", check="code"))
  expect_that(t$check, equals("code"))

  t <- make_target("real", list(command="foo()", cleanup_level="purge"))
  expect_that(t$cleanup_level, equals("purge"))

  ## With dependencies:
  t <- make_target("real", list(command="foo()", depends="a"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends_name, equals("a"))

  t <- make_target("real", list(command="foo(a)"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends_name, equals("a"))

  t <- make_target("real", list(command="foo(a, b=c)"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends_name, equals(c("a", "c")))

  ## Corner case here:
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
              throws_error("Unknown fields in real: other_opt"))
})

test_that("File targets", {
  t <- make_target("foo.csv", list(command="foo()"))
  expect_that(t$type, equals("file"))
  expect_that(t, is_a("target_file"))
  expect_that(t, is_a("target_base"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends_name, equals(character(0)))
  expect_that(t$target_argument, is_null())
  expect_that(target_run_fake(t), equals("foo()"))

  t <- make_target("foo.csv", list(command="foo(a, b, c)"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends_name, equals(c("a", "b", "c")))
  expect_that(t$target_argument, is_null())

  t <- make_target("foo.csv", list(command="foo(a, b, C=c)"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends_name, equals(c("a", "b", "c")))

  t <- make_target("foo.csv", list(command="foo(target_name, b, C=c)"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends_name, equals(c("b", "c")))
  expect_that(as.list(t$command[2]), equals(list("foo.csv")))

  t <- make_target("foo.csv", list(command="foo(name='foo.csv', b, C=c)"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, equals("foo"))
  expect_that(t$depends_name, equals(c("b", "c")))
  expect_that(as.list(t$command[2]), equals(list(name="foo.csv")))
})

test_that("Implicit file targets", {
  expect_that(make_target("code.R", list()),
              throws_error("Must not have a NULL rule"))
  t <- target_new_file_implicit("code.R")
  expect_that(t$type, equals("file"))

  t <- target_new_file_implicit("code.R")
  expect_that(t$name, equals("code.R"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, is_null())
  expect_that(t$depends_name, equals(character(0)))
  expect_that(target_build(t),
              throws_error("Can't build implicit targets"))
  expect_that(target_run(t, NULL), is_null())
  expect_that(target_run_fake(t), is_null())

  expect_that(t <- target_new_file_implicit("file.csv"),
              gives_warning("Creating implicit target for nonexistant"))
  expect_that(t$name, equals("file.csv"))
  expect_that(t$type, equals("file"))
  expect_that(t$rule, is_null())
  ## TODO: empty depends should be character(0), or nonempty lists
  ## should be lists.
  expect_that(t$depends_name, equals(character(0)))
  expect_that(target_build(t),
              throws_error("Can't build implicit targets"))
  expect_that(target_run(t), is_null())
  expect_that(target_run_fake(t), is_null())
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
  ##
  ## auto_figure_prefix works off the *target* name, not the Rmd name
  ## (when different) -- this not not tested.
  t <- make_target("file.md", list(knitr=list(auto_figure_prefix=TRUE)))
  expect_that(t$knitr$auto_figure_prefix, is_true())

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
              throws_error("Unknown fields in file.md"))

  expect_that(make_target("file.md", list(knitr=TRUE,
                                          auto_figure_prefix=TRUE)),
              throws_error("Unknown fields in file.md: auto_figure_prefix"))

  dat <- list(knitr=list(
                options=list(fig.path="foo"),
                auto_figure_prefix=TRUE))
  expect_that(make_target("file.md", dat),
              gives_warning("Ignoring 'auto_figure_prefix'"))
})

## This section is weird and pretty much just a regression test.  I
## hope that this sort of functionality is not actually that useful.
test_that("cleanup", {
  cleanup()
  m <- remake("remake_cleanup_hook.yml")
  t <- m$targets[["clean"]]
  expect_that(length(t$depends_name), equals(2))
  expect_that(t$depends_name, equals(c("data.csv", "tidy")))

  expect_that(file.exists("data.csv"), is_false())
  expect_that(remake_make(m, "clean"),
              shows_message("running post-cleanup hook"))
  expect_that(file.exists("data.csv"), is_true())
  expect_that(remake_make(m, "purge"),
              shows_message("running post-cleanup hook"))
  expect_that(file.exists("data.csv"), is_false())

  cleanup()
  expect_that(remake("remake_cleanup_error.yml"),
              throws_error("Cleanup target commands must have no arguments"))
})

## Things that need activation:
test_that("get/set/archive object targets", {
  cleanup()
  m <- remake("remake.yml")
  remake_make(m, "processed")
  t <- m$targets[["processed"]]
  expect_that(target_get(t, m$store), is_a("data.frame"))
  dep <- dependency_status(t, m$store)

  path <- tempfile()
  dir.create(path)
  archive_export_target(t, m$store, path)

  expect_that(dir(file.path(path, "objects")),
              equals(c("processed", "processed__hash")))
  expect_that(readRDS(file.path(path, "objects", "processed")),
               equals(target_get(t, m$store)))
  expect_that(readLines(file.path(path, "objects", "processed__hash")),
               equals(digest::digest(target_get(t, m$store))))

  name <- paste0(digest::digest(t$name), ".rds")
  expect_that(dir(file.path(path, "db")), equals(name))
  res <- readRDS(file.path(path, "db", name))
  expect_that(res[names(res) != "time"],
              equals(dep[names(dep) != "time"]))
  expect_that(dep$time, is_more_than(res$time))
  file_remove(path, recursive=TRUE)

  ## Set this to rubbish values:
  target_set(t, m$store, "foo")
  expect_that(target_get(t, m$store), equals("foo"))

  remake_remove_target(m, "processed")

  path <- tempfile()
  dir.create(path)
  expect_that(archive_export_target(t, m$store, path),
              throws_error("processed not found in object store"))

  file_remove(path, recursive=TRUE)
})

## Things that need activation:
test_that("get/set/archive file targets", {
  cleanup()
  m <- remake("remake.yml")
  remake_make(m, "plot.pdf")
  t <- m$targets[["plot.pdf"]]
  expect_that(target_get(t, m$store), equals("plot.pdf"))
  dep <- dependency_status(t, m$store)

  path <- tempfile()
  dir.create(path)
  archive_export_target(t, m$store, path)

  md5 <- function(f) unname(tools::md5sum(f))
  expect_that(dir(file.path(path, "files")), equals("plot.pdf"))
  expect_that(md5(file.path(path, "files", "plot.pdf")),
              equals(md5("plot.pdf")))

  name <- paste0(digest::digest(t$name), ".rds")
  expect_that(dir(file.path(path, "db")), equals(name))
  res <- readRDS(file.path(path, "db", name))
  expect_that(res[names(res) != "time"],
              equals(dep[names(dep) != "time"]))
  expect_that(dep$time, is_more_than(res$time))
  file_remove(path, recursive=TRUE)

  remake_remove_target(m, "plot.pdf")

  path <- tempfile()
  dir.create(path)
  expect_that(archive_export_target(t, m$store, path),
              throws_error("plot.pdf not found in file store"))

  file_remove(path, recursive=TRUE)
})

test_that("Error messages", {
  expect_that(remake("remake_invalid.yml"),
              throws_error("While processing target 'all'"))
})

test_that("Targets from calls", {
  t1 <- make_target("a", list(command="foo(b)"))
  t2 <- make_target("a", list(command=quote(foo(b))))
  expect_that(t1, is_identical_to(t2))
})

## Tests of low-level target validity.  Some of these will require
## considerable mocking up.  I'm not doing this via yaml, because that
## seems like a pain :)
context("Targets (low level)")

test_that("reserved names", {
  reserved <- c("target_name", ".")
  expect_equal(sort(reserved), sort(target_reserved_names()))
  for (i in reserved) {
    expect_error(make_target(i), "Target name .+ is reserved")
  }
})

## The simplest target types:
test_that("Fake targets", {
  t <- make_target("a_fake_target", list(type="fake"))

  expect_is(t, "target_fake")
  expect_is(t, "target_base")
  expect_equal(t$type, "fake")

  expect_equal(t$name, "a_fake_target")
  expect_equal(t$depends_name, character(0))
  expect_equal(t$rule, NULL)
  expect_equal(t$cleanup_level, "never")
  expect_equal(t$quiet, FALSE)
  expect_equal(t$check, "all")

  ## For this set of arguments we'd infer a fake target, too:
  t <- make_target("a_fake_target", list())
  expect_equal(t$type, "fake")

  deps <- letters[1:3]
  t <- make_target("a_fake_target", list(depends=deps))
  expect_equal(t$type, "fake")
  expect_equal(t$depends_name, deps)

  ## Same as above, but with packages:
  expect_error(make_target("a_fake_target",
                           list(depends=deps, packages="pkg")),
               "fake targets may not load packages")
})

test_that("Can't do much with fake targets", {
  t <- make_target("a_fake_target", list(type="fake"))
  expect_error(target_get(t), "Not something that can be")
  expect_error(target_set(t), "Not something that can be")
})

test_that("Fake targets (invalid)", {
  expect_error(make_target("fake", list(command="foo()", type="fake")),
               "fake targets must have a NULL rule")
  expect_error(make_target("fake", list(target_argument="foo", type="fake")),
               "Invalid keys: target_argument")
  expect_error(make_target("fake", list(cleanup_level="tidy", type="fake")),
               "Unknown fields in fake: cleanup_level")
  expect_error(make_target("fake", list(other_opt="tidy", type="fake")),
               "Unknown fields in fake: other_opt")
})

test_that("Dependency parsing", {
  ## Empty:
  expect_equal(from_yaml_map_list(yaml_load("[]")), list())

  ## No argument names:
  expect_equal(from_yaml_map_list(yaml_load("[a, b, c]")),
               list("a", "b", "c"))
  expect_equal(from_yaml_map_list(yaml_load("- a\n- b\n- c")),
               list("a", "b", "c"))

  expect_equal(from_yaml_map_list(yaml_load("[A: a, b, c]")),
               list(A="a", "b", "c"))
  expect_equal(from_yaml_map_list(yaml_load("- A: a\n- b\n- c")),
               list(A="a", "b", "c"))
})

test_that("Object target", {
  t <- make_target("real", list(command="foo()"))

  expect_is(t, "target_object")
  expect_is(t, "target_base")
  expect_equal(t$type, "object")

  expect_equal(t$name, "real")
  expect_equal(t$depends_name, character(0))
  expect_equal(t$depends_name, character(0))
  expect_equal(t$rule, "foo")
  expect_equal(t$cleanup_level, "tidy")
  expect_equal(t$quiet, FALSE)
  expect_equal(t$check, "all")

  expect_equal(target_run_fake(t), "real <- foo()")

  ## Passing options:
  t <- make_target("real", list(command="foo()", quiet=TRUE))
  expect_equal(t$quiet, TRUE)

  t <- make_target("real", list(command="foo()", check="code"))
  expect_equal(t$check, "code")

  t <- make_target("real", list(command="foo()", cleanup_level="purge"))
  expect_equal(t$cleanup_level, "purge")

  ## With dependencies:
  t <- make_target("real", list(command="foo()", depends="a"))
  expect_equal(t$rule, "foo")
  expect_equal(t$depends_name, "a")

  t <- make_target("real", list(command="foo(a)"))
  expect_equal(t$rule, "foo")
  expect_equal(t$depends_name, "a")

  t <- make_target("real", list(command="foo(a, b=c)"))
  expect_equal(t$rule, "foo")
  expect_equal(t$depends_name, c("a", "c"))

  ## Corner case here:
  t <- make_target("code.R", list(command="foo()", type="object"))
  expect_equal(t$type, "object")
})

## TODO: These error messages are super inconsistent.
test_that("Object target (invalid)", {
  expect_error(make_target("foo", "bar"),
               "While processing target 'foo':")
  expect_error(make_target("foo", "bar"),
               "target data must be named")
  expect_error(make_target("foo", c(name="bar")),
               "target data must be a list")

  ## This is actually hard to achive:
  expect_error(make_target("real", list(type="object")),
               "Must not have a NULL rule")

  expect_error(make_target("real", list(command="foo()", target_argument=1)),
               "Invalid keys: target_argument")
  expect_error(make_target("real", list(rule="foo")),
               "Invalid keys: rule")

  expect_error(make_target("real", list(command="foo()", quiet="quiet")),
               "quiet must be logical")
  expect_error(make_target("real", list(command="foo()", quiet=c(TRUE, TRUE))),
               "quiet must be a scalar")

  expect_error(make_target("real", list(command="foo()",
                                        cleanup_level="purge2")),
               "cleanup_level must be one")

  expect_error(make_target("real", list(command="foo()", check="exists2")),
               "check must be one")

  expect_error(make_target("real", list(command="foo()", other_opt="tidy")),
               "Unknown fields in real: other_opt")
})

test_that("File targets", {
  t <- make_target("foo.csv", list(command="foo()"))
  expect_equal(t$type, "file")
  expect_is(t, "target_file")
  expect_is(t, "target_base")
  expect_equal(t$rule, "foo")
  expect_equal(t$depends_name, character(0))
  expect_null(t$target_argument)
  expect_equal(target_run_fake(t), "foo()")

  t <- make_target("foo.csv", list(command="foo(a, b, c)"))
  expect_equal(t$type, "file")
  expect_equal(t$rule, "foo")
  expect_equal(t$depends_name, c("a", "b", "c"))
  expect_null(t$target_argument)

  t <- make_target("foo.csv", list(command="foo(a, b, C=c)"))
  expect_equal(t$type, "file")
  expect_equal(t$rule, "foo")
  expect_equal(t$depends_name, c("a", "b", "c"))

  t <- make_target("foo.csv", list(command="foo(target_name, b, C=c)"))
  expect_equal(t$type, "file")
  expect_equal(t$rule, "foo")
  expect_equal(t$depends_name, c("b", "c"))
  expect_equal(as.list(t$command[2]), list("foo.csv"))

  t <- make_target("foo.csv", list(command="foo(name='foo.csv', b, C=c)"))
  expect_equal(t$type, "file")
  expect_equal(t$rule, "foo")
  expect_equal(t$depends_name, c("b", "c"))
  expect_equal(as.list(t$command[2]), list(name="foo.csv"))
})

test_that("Implicit file targets", {
  expect_error(make_target("code.R", list()),
               "Must not have a NULL rule")
  t <- target_new_file_implicit("code.R")
  expect_equal(t$type, "file")

  t <- target_new_file_implicit("code.R")
  expect_equal(t$name, "code.R")
  expect_equal(t$type, "file")
  expect_null(t$rule)
  expect_equal(t$depends_name, character(0))
  expect_error(target_build(t),
               "Can't build implicit targets")
  expect_null(target_run(t, NULL))
  expect_null(target_run_fake(t))

  expect_warning(t <- target_new_file_implicit("file.csv"),
                 "Creating implicit target for nonexistant")
  expect_equal(t$name, "file.csv")
  expect_equal(t$type, "file")
  expect_null(t$rule)
  ## TODO: empty depends should be character(0), or nonempty lists
  ## should be lists.
  expect_equal(t$depends_name, character(0))
  expect_error(target_build(t),
               "Can't build implicit targets")
  expect_null(target_run(t))
  expect_null(target_run_fake(t))
})

test_that("knitr", {
  t <- make_target("file.md", list(type="knitr"))
  expect_is(t, "target_knitr")
  ## TODO: Not sure that this is correct (see elsewhere for plot though)
  expect_equal(t$type, "file")

  ## Inferred correct input:
  expect_equal(t$knitr$input, "file.Rmd")

  ## No options set
  expect_equal(t$knitr$options, list(error=FALSE))

  ## Allow knitr in opts:
  t <- make_target("file.md", list(knitr=TRUE))
  expect_is(t, "target_knitr")

  ## Only valid option is "input":
  t <- make_target("file.md", list(knitr=list(input="foo.Rmd")))
  expect_equal(t$knitr$input, "foo.Rmd")

  ## Test auto_figure_prefix:
  ##
  ## auto_figure_prefix works off the *target* name, not the Rmd name
  ## (when different) -- this not not tested.
  t <- make_target("file.md", list(knitr=list(auto_figure_prefix=TRUE)))
  expect_true(t$knitr$auto_figure_prefix)

  prefix <- "figures/file-"
  t <- make_target("file.md",
                   list(knitr=list(options=list(fig.path=prefix))))
  expect_equal(t$knitr$options$fig.path, prefix)

  ## Quiet by default:
  expect_true(t$quiet)
  expect_true(make_target("file.md", list(quiet=TRUE, type="knitr"))$quiet)
  expect_false(make_target("file.md", list(quiet=FALSE, type="knitr"))$quiet)

  ## This might change
  expect_equal(t$rule, ".__knitr__")
})

test_that("knitr (invalid)", {
  expect_error(make_target("file.xmd", list(knitr=TRUE)),
               "Target must end in .md")

  expect_error(make_target("file.md", list(command="fn()", type="knitr")),
               "knitr targets must have a NULL rule")

  expect_error(make_target("file.md", list(command="fn()", knitr=TRUE)),
               "knitr targets must have a NULL rule")
  ## Chains fail because commands fail:
  expect_error(make_target("file.md",
                           list(command=c("foo()", "bar(.)"), knitr=TRUE)),
               "commands must be scalar")

  expect_error(make_target("file.md", list(quiet="yes please",
                                           type="knitr"))$quiet,
               "quiet must be logical")

  expect_error(make_target("file.md", list(unknown="opt", type="knitr")),
               "Unknown fields in file.md")

  expect_error(make_target("file.md", list(knitr=TRUE,
                                           auto_figure_prefix=TRUE)),
               "Unknown fields in file.md: auto_figure_prefix")

  dat <- list(knitr=list(
                options=list(fig.path="foo"),
                auto_figure_prefix=TRUE))
  expect_warning(make_target("file.md", dat),
                 "Ignoring 'auto_figure_prefix'")
})

test_that("download", {
  url <- "http://whatever"
  t <- make_target("file", list(download=url))
  expect_equal(class(t),
               c("target_download", "target_file", "target_base"))
  expect_equal(t$download, url)
  expect_equal(t$check, "exists")
  expect_equal(t$cleanup_level, "purge")
  expect_equal(t$type, "file")
})

test_that("download (invalid)", {
  expect_error(make_target("file", list(download=NULL)),
               "download must be a scalar")
  expect_error(make_target("file", list(download=character(0))),
               "download must be a scalar")
  expect_error(make_target("file", list(download=c("a", "b"))),
               "download must be a scalar")
  expect_error(make_target("file", list(download=TRUE)),
               "download must be character")

  expect_error(make_target("file", list(download="www.whatever")),
               "does not look like a")
  expect_error(make_target("file", list(download="ftp://www.whatever")),
               "does not look like a")
  expect_error(make_target("file", list(download="file://www.whatever")),
               "does not look like a")
})

## This section is weird and pretty much just a regression test.  I
## hope that this sort of functionality is not actually that useful.
test_that("cleanup", {
  cleanup()
  m <- remake("remake_cleanup_hook.yml")
  t <- m$targets[["clean"]]
  expect_equal(length(t$depends_name), 2)
  expect_equal(t$depends_name, c("data.csv", "tidy"))

  expect_false(file.exists("data.csv"))
  expect_message(remake_make(m, "clean"),
                 "running post-cleanup hook")
  expect_true(file.exists("data.csv"))
  expect_message(remake_make(m, "purge"),
                 "running post-cleanup hook")
  expect_false(file.exists("data.csv"))

  cleanup()
  expect_error(remake("remake_cleanup_error.yml"),
               "Cleanup target commands must have no arguments")
})

## Things that need activation:
test_that("get/set/archive object targets", {
  cleanup()
  m <- remake("remake.yml")
  remake_make(m, "processed")
  t <- m$targets[["processed"]]
  expect_is(target_get(t, m$store), "data.frame")
  dep <- dependency_status(t, m$store)

  path <- tempfile()
  dir.create(path)
  archive_export_target(t, m$store, path)

  ## TODO: now that this has moved into storr, we don't need to test
  ## for implementation details here.
  expect_equal(dir(file.path(path, "objects")),
               c("config", "data", "keys"))
  expect_equal(dir(file.path(path, "objects", "keys", "objects")),
               storr::encode64("processed"))
  expect_equal(readLines(file.path(path, "objects", "keys", "objects",
                                   storr::encode64("processed"))),
               digest::digest(target_get(t, m$store)))

  st_db <- storr::storr_rds(file.path(path, "objects"),
                            default_namespace="remake_db",
                            mangle_key=TRUE)

  expect_equal(st_db$list(), t$name)
  res <- st_db$get(t$name)
  expect_equal(res[names(res) != "time"],
               dep[names(dep) != "time"])
  expect_gt(as.numeric(dep$time), as.numeric(res$time))
  file_remove(path, recursive=TRUE)

  ## Set this to rubbish values:
  target_set(t, m$store, "foo")
  expect_equal(target_get(t, m$store), "foo")

  remake_remove_target(m, "processed")

  path <- tempfile()
  dir.create(path)
  expect_error(archive_export_target(t, m$store, path),
               "processed not found in object store")

  file_remove(path, recursive=TRUE)
})

## Things that need activation:
test_that("get/set/archive file targets", {
  cleanup()
  m <- remake("remake.yml")
  remake_make(m, "plot.pdf")
  t <- m$targets[["plot.pdf"]]
  expect_equal(target_get(t, m$store), "plot.pdf")
  dep <- dependency_status(t, m$store)

  path <- tempfile()
  dir.create(path)
  archive_export_target(t, m$store, path)

  md5 <- function(f) unname(tools::md5sum(f))
  expect_equal(dir(file.path(path, "files")), "plot.pdf")
  expect_equal(md5(file.path(path, "files", "plot.pdf")),
               md5("plot.pdf"))

  st_db <- storr::storr_rds(file.path(path, "objects"),
                            default_namespace="remake_db",
                            mangle_key=TRUE)

  expect_equal(st_db$list(), t$name)
  res <- st_db$get(t$name)
  expect_equal(res[names(res) != "time"],
               dep[names(dep) != "time"])
  expect_gt(as.numeric(dep$time), as.numeric(res$time))
  file_remove(path, recursive=TRUE)

  remake_remove_target(m, "plot.pdf")

  path <- tempfile()
  dir.create(path)
  expect_error(archive_export_target(t, m$store, path),
               "plot.pdf not found in file store")

  file_remove(path, recursive=TRUE)
})

test_that("Error messages", {
  expect_error(remake("remake_invalid.yml"),
               "While processing target 'all'")
})

test_that("Targets from calls", {
  t1 <- make_target("a", list(command="foo(b)"))
  t2 <- make_target("a", list(command=quote(foo(b))))
  expect_identical(t1, t2)
})

test_that("Extensions", {
  expect_identical(file_extensions(), tolower(file_extensions()))
})

## NOTE: here, file_extensions is either NULL or a _complete_ list;
## this differs from the logic used in the yml as this is processed a
## little.
test_that("custom file extensions", {
  expect_is(make_target("a.phy", list(command="foo()")),
            "target_object")
  expect_is(make_target("a.phy", list(command="foo()"), file_extensions="phy"),
            "target_file")
})

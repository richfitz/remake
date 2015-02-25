context("API functions")

test_that("list_targets", {
  ## NOTE: The sort order here is not necessarily stable
  expect_that(list_targets(),
              equals(c("all", "data.csv", "processed", "plot.pdf")))

  expect_that(list_targets(type="object"),
              equals("processed"))
  expect_that(list_targets(type="file"),
              equals(c("data.csv", "plot.pdf")))
  expect_that(list_targets(type="fake"),
              equals(c("all")))

  expect_that(list_targets(type="cleanup"),
              gives_warning("cleanup type listed in type, but also ignored"))
  expect_that(suppressWarnings(list_targets(type="cleanup")),
              equals(character(0)))
  expect_that(list_targets(type="cleanup", include_cleanup_targets=TRUE),
              equals(cleanup_target_names()))

  ## Two types:
  expect_that(list_targets(type=c("object", "fake")),
              equals(c("all", "processed")))
  ## No types
  expect_that(list_targets(type=character(0)),
              equals(character(0)))

  ## With implicit files:
  fake_empty_file("data.csv")
  on.exit(file_remove("data.csv"))
  expect_that(list_targets("remake2.yml"),
              equals(c("processed", "plot.pdf")))
  expect_that(list_targets("remake2.yml", include_implicit_files=TRUE),
              equals(c("processed", "plot.pdf", "data.csv")))

  ## Chain rules
  cmp <- c("manual_pt1", "manual_pt2", "manual", "chained")
  expect_that(list_targets("chain.yml"), equals(cmp))
  expect_that(list_targets("chain.yml", include_chain_intermediates=TRUE),
              equals(c(cmp, "chained{1}", "chained{2}")))
})

test_that("list_dependencies", {
  ## In topological order!
  expect_that(list_dependencies("all"),
              equals(c("data.csv", "processed", "plot.pdf", "all")))
  expect_that(list_dependencies("processed"),
              equals(c("data.csv", "processed")))
  expect_that(list_dependencies("data.csv"),
              equals(c("data.csv")))
  ## Cleanup targets usually get filtered:
  expect_that(list_dependencies("purge"),
              equals(character(0)))
  expect_that(list_dependencies("purge", include_cleanup_targets=TRUE),
              equals(c("tidy", "clean", "purge")))

  ## Chains:
  expect_that(list_dependencies("chained", remake_file="chain.yml"),
              equals("chained"))
  expect_that(list_dependencies("chained",
                                include_chain_intermediates=TRUE,
                                remake_file="chain.yml"),
              equals(c("chained{1}", "chained{2}", "chained")))

  ## Implicit targets:
  expect_that(list_dependencies("plot.pdf", remake_file="remake2.yml"),
              equals(c("processed", "plot.pdf")))
  expect_that(list_dependencies("plot.pdf",
                                include_implicit_files=TRUE,
                                remake_file="remake2.yml"),
              equals(c("data.csv", "processed", "plot.pdf")))

  ## Filter by type:
  expect_that(list_dependencies("all", type="object"),
              equals("processed"))
  expect_that(list_dependencies("all", type="fake"),
              equals("all"))
  expect_that(list_dependencies("all", type="file"),
              equals(c("data.csv", "plot.pdf")))
  expect_that(list_dependencies("all", type="cleanup"),
              gives_warning("cleanup type listed"))
  expect_that(list_dependencies("all", type="cleanup",
                                include_cleanup_targets=TRUE),
              equals(character(0)))

  ## Corner case:
  expect_that(list_dependencies(character(0)),
              equals(character(0)))
  ## The message is not ideal, but I'm OK with an error:
  expect_that(list_dependencies(NA),
              throws_error())

  ## Invalid targets:
  expect_that(list_dependencies("no_such_target"),
              throws_error("Unknown target"))
})

test_that("is_current", {
  ## Verbose:
  cleanup()
  expect_that(is_current("all"), not(shows_message()))
  cleanup()
  expect_that(is_current("all", verbose=TRUE), shows_message("LOAD"))

  ## Nonexistant targets throw error
  expect_that(is_current("no_such_target"),
              throws_error("No such target"))
  expect_that(is_current(c("no_such_target", "another")),
              throws_error("No such target"))

  ## Zero length target
  expect_that(is_current(character(0)), equals(logical(0)))

  targets <- list_targets()
  expect_that(is_current("all"), equals(FALSE))
  expect_that(is_current(targets), equals(rep(FALSE, length(targets))))

  make("data.csv")
  expect_that(is_current("data.csv"), is_true())
  expect_that(is_current(setdiff(targets, "data.csv")),
              equals(rep(FALSE, length(targets) - 1)))

  make()
  ## Fake targets never up to date.
  ## TODO: I wonder if this is actually the best behaviour; we could
  ## be less simple-minded and store signaures of all dependencies.
  ## The issue is what the hash of a fake target is.  The current
  ## situation at least should trigger builds.
  expect_that(is_current("all"), is_false())
  expect_that(is_current(setdiff(targets, "all")),
              equals(rep(TRUE, length(targets) - 1)))

  ## Some of the examples from test-check-current.R
  cleanup()

  expect_that(is_current("data.csv", remake_file="remake_check.yml"),
              is_false())
  expect_that(is_current("data.csv", "exists", remake_file="remake_check.yml"),
              is_false())

  make("plot.pdf", remake_file="remake_check.yml")

  expect_that(is_current("data.csv", remake_file="remake_check.yml"),
              is_true())
  expect_that(is_current("data.csv", "exists", remake_file="remake_check.yml"),
              is_true())

  expect_that(is_current("data.csv", remake_file="remake_check.yml"),
              is_true())
  expect_that(is_current("data.csv", "exists", remake_file="remake_check.yml"),
              is_true())

  expect_that(is_current("processed", remake_file="remake_check.yml"),
              is_true())
  expect_that(is_current("processed", "exists", remake_file="remake_check.yml"),
              is_true())

  expect_that(is_current("plot.pdf", remake_file="remake_check.yml"),
              is_true())
  expect_that(is_current("plot.pdf", "exists", remake_file="remake_check.yml"),
              is_true())
})

test_that("auto_gitignore", {
  ## NOTE: .remake is ignored by the repo .gitignore file; these tests
  ## will fail if not running there.  It will also fail if the global
  ## gitignore is set and ignores anything here (which is moderately
  ## unlikely).  In any case, these tests are horribly fragile :(
  has_git <- git_exists()
  ignore_remake <- git_ignores(".remake")
  x <- if (ignore_remake) character(0) else ".remake"

  tmp <- backup(".gitignore")
  on.exit(restore(".gitignore", tmp))

  file_remove(".gitignore")
  expect_that(auto_gitignore(dry_run=TRUE),
              equals(c(x, "data.csv", "plot.pdf")))

  writeLines("plot.pdf", ".gitignore")
  expect_that(auto_gitignore(dry_run=TRUE),
              equals(c(x, "data.csv")))
  expect_that(auto_gitignore("remake2.yml", dry_run=TRUE),
              equals(c(x, character(0))))
  expect_that(auto_gitignore(dry_run=TRUE, check_git=FALSE),
              equals(c(".remake", "data.csv")))

  ## Glob:
  writeLines("*.pdf", ".gitignore")
  expect_that(auto_gitignore(dry_run=TRUE),
              equals(c(x, "data.csv", if (!has_git) "plot.pdf")))
  expect_that(auto_gitignore(check_git=FALSE, dry_run=TRUE),
              equals(c(".remake", "data.csv", "plot.pdf")))

  file_remove(".gitignore")
  str <- auto_gitignore()
  expect_that(readLines(".gitignore"), is_identical_to(str))
  expect_that(auto_gitignore(), equals(character(0)))
  expect_that(readLines(".gitignore"), is_identical_to(str))
})

## This one is pretty well tested via remake_environment so probably
## don't need to go crazy here.
test_that("make_environment", {
  cleanup()
  e <- make_environment()
  expect_that(ls(e),
              equals(c("clean_hook", "do_plot", "download_data",
                       "myplot", "process_data")))
  ## TODO: Throw a better error:
  expect_that(make_environment("processed"),
              throws_error("key processed not found in object store"))
  make()
  expect_that("processed" %in% ls(make_environment("processed")),
              is_true())

  make(c("manual", "chained"), remake_file="chain.yml")
  ## This *should* throw an error!  But because we share an object
  ## store it works just fine.  This is more reason for different
  ## stores to have different prefixes.
  ## e <- make_environment(c("manual", "chained"))
  e <- make_environment(c("manual", "chained"),
                        dependencies=TRUE,
                        copy_functions=FALSE,
                        remake_file="chain.yml")

  expect_that(sort(ls(e)),
              equals(sort(c("chained", "manual", "manual_pt1",
                            "manual_pt2"))))
})

test_that("fetch", {
  cleanup()

  expect_that(fetch("all"),
              throws_error("Can only fetch object targets"))
  expect_that(fetch("clean"),
              throws_error("Can only fetch object targets"))
  expect_that(fetch("data.csv"),
              throws_error("Can only fetch object targets"))
  expect_that(fetch("plot.pdf"),
              throws_error("Can only fetch object targets"))
  expect_that(fetch("nosuchtarget"),
              throws_error("No such target"))
  expect_that(fetch(c("a", "b")),
              throws_error("must be a scalar"))
  expect_that(fetch(NULL),
              throws_error("must be a scalar"))
  expect_that(fetch(1L),
              throws_error("must be character"))

  expect_that(fetch("processed"),
              throws_error("has not been made"))
  make()
  d <- fetch("processed")
  expect_that(d, is_a("data.frame"))
  obj <- remake()
  expect_that(d, is_identical_to(obj$store$objects$get("processed")))

  ## Then, we'll invalidate data.csv so that things are out of date:
  file.remove("data.csv")
  expect_that(is_current("processed"), is_false())
  d2 <- fetch("processed")
  expect_that(d2, is_identical_to(d))

  expect_that(fetch("processed", require_current=TRUE),
              throws_error("Object is out of date"))
})

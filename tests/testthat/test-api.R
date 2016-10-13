context("API functions")

test_that("list_targets", {
  ## NOTE: The sort order here is not necessarily stable
  expect_equal(sort(list_targets()),
               sort(c("all", "data.csv", "processed", "plot.pdf")))

  expect_equal(list_targets(type="object"), "processed")
  expect_equal(list_targets(type="file"), c("data.csv", "plot.pdf"))
  expect_equal(list_targets(type="fake"), c("all"))

  expect_warning(list_targets(type="cleanup"),
                 "cleanup type listed in type, but also ignored")
  expect_equal(suppressWarnings(list_targets(type="cleanup")),
               character(0))
  expect_equal(list_targets(type="cleanup", include_cleanup_targets=TRUE),
               cleanup_target_names())

  ## Two types:
  expect_equal(list_targets(type=c("object", "fake")),
               c("all", "processed"))
  ## No types
  expect_equal(list_targets(type=character(0)),
               character(0))

  ## With implicit files:
  fake_empty_file("data.csv")
  on.exit(file_remove("data.csv"))
  expect_equal(list_targets("remake2.yml"),
               c("processed", "plot.pdf"))
  expect_equal(list_targets("remake2.yml", include_implicit_files=TRUE),
               c("processed", "plot.pdf", "data.csv"))
})

test_that("list_dependencies", {
  ## In topological order!
  expect_equal(list_dependencies("all"),
               c("data.csv", "processed", "plot.pdf", "all"))
  expect_equal(list_dependencies("processed"),
               c("data.csv", "processed"))
  expect_equal(list_dependencies("data.csv"), c("data.csv"))
  ## Cleanup targets usually get filtered:
  expect_equal(list_dependencies("purge"), character(0))
  expect_equal(list_dependencies("purge", include_cleanup_targets=TRUE),
               c("tidy", "clean", "purge"))

  ## Implicit targets:
  expect_equal(list_dependencies("plot.pdf", remake_file="remake2.yml"),
               c("processed", "plot.pdf"))
  expect_equal(list_dependencies("plot.pdf",
                                 include_implicit_files=TRUE,
                                 remake_file="remake2.yml"),
               c("data.csv", "processed", "plot.pdf"))

  ## Filter by type:
  expect_equal(list_dependencies("all", type="object"), "processed")
  expect_equal(list_dependencies("all", type="fake"), "all")
  expect_equal(list_dependencies("all", type="file"),
               c("data.csv", "plot.pdf"))
  expect_warning(list_dependencies("all", type="cleanup"),
                 "cleanup type listed")
  expect_equal(list_dependencies("all", type="cleanup",
                                 include_cleanup_targets=TRUE),
               character(0))

  ## Corner case:
  expect_equal(list_dependencies(character(0)), character(0))
  ## The message is not ideal, but I'm OK with an error:
  expect_error(list_dependencies(NA), "Unknown target: NA")

  ## Invalid targets:
  expect_error(list_dependencies("no_such_target"),
               "Unknown target")

  cleanup()
  expect_warning(
    list_dependencies("processed", remake_file="remake_missing_package.yml"),
    NA) # "Missing warning"
})

test_that("is_current", {
  ## Verbose:
  cleanup()
  expect_silent(is_current("all"))
  cleanup()
  expect_message(is_current("all", verbose=TRUE), "LOAD")

  ## Nonexistant targets throw error
  expect_error(is_current("no_such_target"),
               "No such target")
  expect_error(is_current(c("no_such_target", "another")),
               "No such target")

  ## Zero length target
  expect_equal(is_current(character(0)), logical(0))

  targets <- list_targets()
  expect_false(is_current("all"))
  expect_equal(is_current(targets), rep(FALSE, length(targets)))

  make("data.csv")
  expect_true(is_current("data.csv"))
  expect_equal(is_current(setdiff(targets, "data.csv")),
               rep(FALSE, length(targets) - 1))

  make()
  ## Fake targets never up to date.
  ## TODO: I wonder if this is actually the best behaviour; we could
  ## be less simple-minded and store signaures of all dependencies.
  ## The issue is what the hash of a fake target is.  The current
  ## situation at least should trigger builds.
  expect_false(is_current("all"))
  expect_equal(is_current(setdiff(targets, "all")),
               rep(TRUE, length(targets) - 1))

  ## Some of the examples from test-check-current.R
  cleanup()

  expect_false(is_current("data.csv", remake_file="remake_check.yml"))
  expect_false(is_current("data.csv", "exists", remake_file="remake_check.yml"))

  make("plot.pdf", remake_file="remake_check.yml")

  expect_true(is_current("data.csv", remake_file="remake_check.yml"))
  expect_true(is_current("data.csv", "exists", remake_file="remake_check.yml"))

  expect_true(is_current("data.csv", remake_file="remake_check.yml"))
  expect_true(is_current("data.csv", "exists", remake_file="remake_check.yml"))

  expect_true(is_current("processed", remake_file="remake_check.yml"))
  expect_true(is_current("processed", "exists", remake_file="remake_check.yml"))

  expect_true(is_current("plot.pdf", remake_file="remake_check.yml"))
  expect_true(is_current("plot.pdf", "exists", remake_file="remake_check.yml"))

  cleanup()
  expect_warning(is_current("processed",
                            remake_file="remake_missing_package.yml"),
                 "Some packages are missing")
  expect_warning(is_current("processed", allow_missing_packages=TRUE,
                            remake_file="remake_missing_package.yml"),
                 NA)
})

test_that("auto_gitignore", {
  ## NOTE: .remake is ignored by the repo .gitignore file; these tests
  ## will fail if not running there.  It will also fail if the global
  ## gitignore is set and ignores anything here (which is moderately
  ## unlikely).  In any case, these tests are horribly fragile :(
  has_git <- git_exists()

  tmp <- backup(".gitignore")
  on.exit(restore(".gitignore", tmp))

  ignores_pdf <- function(file) grepl("[.]pdf$", file)
  ignores_all <- function(file) rep_along(TRUE, file)

  with_mock("remake::git_ignores"=ignores_pdf, "remake::git_exists"=function() TRUE, {
    file_remove(".gitignore")
    expect_equal(auto_gitignore(dry_run=TRUE),
                 c(".remake", "data.csv"))

    writeLines("plot.pdf", ".gitignore")
    expect_equal(auto_gitignore(dry_run=TRUE),
                 c(".remake", "data.csv"))
    expect_equal(suppressWarnings(auto_gitignore("remake2.yml", dry_run=TRUE)),
                 c(".remake"))
    expect_equal(auto_gitignore(dry_run=TRUE, check_git=FALSE),
                 c(".remake", "data.csv"))

    ## Glob:
    writeLines("*.pdf", ".gitignore")
    expect_equal(auto_gitignore(dry_run=TRUE),
                 c(".remake", "data.csv"))
    expect_equal(auto_gitignore(check_git=FALSE, dry_run=TRUE),
                 c(".remake", "data.csv", "plot.pdf"))

    file_remove(".gitignore")
    str <- auto_gitignore()
    expect_identical(readLines(".gitignore"), str)
  })

  with_mock("remake::git_ignores"=ignores_all, "remake::git_exists"=function() TRUE, {
    expect_equal(auto_gitignore(), character(0))
    expect_identical(readLines(".gitignore"), str)
  })
})

## This one is pretty well tested via remake_environment so probably
## don't need to go crazy here.
test_that("make_environment", {
  cleanup()
  e <- make_environment()
  expect_equal(ls(e),
               c("clean_hook", "do_plot", "download_data",
                 "myplot", "process_data"))
  ## TODO: Throw a better error:
  expect_error(make_environment("processed"),
               "key 'processed' ('objects') not found", fixed=TRUE)
  make()
  expect_true("processed" %in% ls(make_environment("processed")))
})

test_that("fetch", {
  cleanup()

  expect_error(fetch("all"), "Can only fetch object targets")
  expect_error(fetch("clean"), "Can only fetch object targets")
  expect_error(fetch("data.csv"), "Can only fetch object targets")
  expect_error(fetch("plot.pdf"), "Can only fetch object targets")
  expect_error(fetch("nosuchtarget"), "No such target")
  expect_error(fetch(c("a", "b")), "must be a scalar")
  expect_error(fetch(NULL), "must be a scalar")
  expect_error(fetch(1L), "must be character")

  expect_error(fetch("processed"), "has not been made")
  make()
  d <- fetch("processed")
  expect_is(d, "data.frame")
  obj <- remake()
  expect_identical(d, obj$store$objects$get("processed"))

  ## Then, we'll invalidate data.csv so that things are out of date:
  file.remove("data.csv")
  expect_false(is_current("processed"))
  d2 <- fetch("processed")
  expect_identical(d2, d)

  expect_error(fetch("processed", require_current=TRUE),
               "Object is out of date")
})

test_that("delete", {
  cleanup()
  make()

  expect_error(delete("all"),
               "Not something that can be deleted")
  expect_message(delete("all", dependencies=TRUE),
                 "DEL")
  expect_false(file.exists("data.csv"))

  make("data.csv")
  expect_message(delete("data.csv"), "DEL")
  expect_false(any(grepl("DEL", capture_messages(delete("data.csv")))))

  make("data.csv")
  expect_message(delete("data.csv", verbose=FALSE), NA)
  expect_false(file.exists("data.csv"))

  ## Different remake file
  cleanup()
  make(remake_file="knitr.yml")
  expect_true(file.exists("knitr.md"))
  delete("knitr.md", remake_file="knitr.yml")
  expect_false(file.exists("knitr.md"))
})

test_that("dump_environment", {
  cleanup()
  env <- new.env(parent=.GlobalEnv)
  dump_environment(env)
  expect_true("do_plot" %in% ls(env))
  expect_false("processed" %in% ls(env))
  make()
  expect_false("processed" %in% ls(env))
  dump_environment(env)
  expect_true("processed" %in% ls(env))
  make("clean")
  expect_true("processed" %in% ls(env))

  cleanup()
  expect_error(suppressWarnings(
    dump_environment(env, remake_file="remake_missing_package.yml")),
    "Some packages are missing")
  expect_error(dump_environment(env,
                                allow_missing_packages=TRUE,
                                remake_file="remake_missing_package.yml"),
               NA)
  expect_error(dump_environment(env,
                                allow_missing_packages=TRUE,
                                remake_file="remake_missing_package.yml"),
               NA)
})

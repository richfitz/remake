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

test_that("auto_gitignore", {
  ## NOTE: .remake is ignored by the repo .gitignore file; these tests
  ## will fail if not running there.  It will also fail if the global
  ## gitignore is set and ignores anything here (which is moderately
  ## unlikely).
  ignore_remake <- try(git_ignores(".remake"), silent=TRUE)
  if (inherits(ignore_remake, "try-error")) {
    skip("Looks like git is not installed")
  }
  x <- if (ignore_remake) character(0) else ".remake"

  tmp <- backup(".gitignore")
  on.exit(restore(".gitignore", tmp))

  file.remove(".gitignore")
  expect_that(auto_gitignore(dry_run=TRUE),
              equals(c(x, "data.csv", "plot.pdf")))
  writeLines("plot.pdf", ".gitignore")
  expect_that(auto_gitignore(dry_run=TRUE),
              equals(c(x, "data.csv")))
  expect_that(auto_gitignore("remake2.yml", dry_run=TRUE),
              equals(c(x, character(0))))

  writeLines("*.pdf", ".gitignore")
  expect_that(auto_gitignore(dry_run=TRUE),
              equals(c(x, "data.csv")))
  expect_that(auto_gitignore(check_git=FALSE, dry_run=TRUE),
              equals(c(".remake", "data.csv", "plot.pdf")))

  file.remove(".gitignore")
  str <- auto_gitignore()
  expect_that(readLines(".gitignore"), is_identical_to(str))
  expect_that(auto_gitignore(), equals(character(0)))
  expect_that(readLines(".gitignore"), is_identical_to(str))
})

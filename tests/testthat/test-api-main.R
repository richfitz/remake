context("API: Main script")

test_that("Default arguments", {
  ## Defaults:
  opts <- remake_main_parse_args(character(0))
  expect_equal(opts$options$remake_file, "remake.yml")
  expect_equal(opts$options$quiet, FALSE)
  expect_equal(opts$options$list_targets, FALSE)
  expect_equal(opts$options$script, FALSE)
  expect_equal(opts$options$script_file, NULL)
  expect_equal(opts$options$version, FALSE)
  expect_equal(opts$options$help, FALSE)
  expect_equal(opts$options$install_missing_packages, FALSE)
  expect_equal(opts$args, character(0))

  ## No unknown options:
  known_opts <- c("help", "list_targets", "quiet", "remake_file",
                  "script", "version", "install_missing_packages")
  expect_equal(sort(names(opts$options)), sort(known_opts))
})

test_that("Misspelt arguments", {
  expect_error(remake_main_parse_args("--quit"),
               'Invalid targets: "--quit"')
  expect_error(remake_main_parse_args(c("--foo", "--bar", "baz")),
               'Invalid targets: "--foo", "--bar"')
})

test_that("Version", {
  expect_message(main("--version"),
                 paste0("remake version ", packageVersion("remake")))
})

test_that("List targets", {
  cmp <- list_targets()
  expect_message(main("--list-targets"),
                 paste(cmp, collapse="\n"))
})

test_that("Plain", {
  cleanup()
  expect_false(file.exists("plot.pdf"))
  expect_message(main(character(0)), "] all", fixed=TRUE)
  expect_true(file.exists("plot.pdf"))
  ## Quiet:
  expect_silent(main("-q"))
})

test_that("Multiple targets", {
  cleanup()
  dat <- evaluate_promise(main(c("processed", "data.csv")))
  ## TODO: This would be heaps easier if we logged what we did
  ## somewhere.
  expected <- c(
    "[  LOAD ]",
    "[  READ ]", # sources
    "<  MAKE > processed",
    "<  MAKE > data.csv",
    "[ BUILD ] data.csv",
    "[  READ ]", # packages
    "[ BUILD ] processed")
  expect_equal(length(dat$messages), length(expected))
  expect_equal(substr(dat$messages, 1, nchar(expected)), expected)
})

test_that("Different remake file", {
  cmp <- list_targets("modular.yml")
  expect_message(main(c("--list-targets", "-f", "modular.yml")),
                 paste(cmp, collapse="\n"))
  ## This style of option parsing does not work:
  ## expect_message(main(c("--list-targets", "-fmodular.yml")),
  ##                paste(cmp, collapse="\n"))
  expect_message(main(c("--list-targets", "--file=modular.yml")),
                 paste(cmp, collapse="\n"))
  expect_message(main(c("--list-targets", "--file", "modular.yml")),
                 paste(cmp, collapse="\n"))
})

test_that("Script", {
  cleanup()
  expect_message(capture.output(main(c("-s"))), "LOAD")
  ## With quiet:
  expect_message(capture.output(main(c("-s", "-q"))), NA)
  expect_equal(capture.output(main(c("-s", "-q"))), make_script())
  tmp <- tempfile()
  main(c("-s", "-q", "--script-file", tmp))
  expect_equal(readLines(tmp), make_script())
})

test_that("Installation", {
  cleanup()
  expect_false(file.exists("remake"))
  install_remake(".")
  expect_true(file.exists("remake"))

  ## TODO: I don't know why this is not working; it works fine
  ## interactively.  It looks like when running non-interactively
  ## we're not setting TERM to a dumb type so colours are getting
  ## mixed in.
  ##
  ## TODO: On top of that, this really requires *installing* remake,
  ## and that's not something we can do on CRAN.
  ##
  ## str <- system2("./remake", stdout=TRUE, stderr=TRUE)
  ## if (interactive()) {
  ##   expect_true(any(grepl("[ BUILD ] plot.pdf", str, fixed=TRUE)))
  ## }

  expect_error(install_remake("."), "already exists")
  expect_error(install_remake(".", TRUE), NA)

  ## Using internal access:
  expect_true("remake:::main()" %in% readLines("remake"))
  ## Working around methods:
  expect_true("library(methods)" %in% readLines("remake"))
})

## TODO: Test --install-missing-targets:
##   1. Installs targets
##   2. Runs no targets unless explicitly given

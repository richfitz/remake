context("API: Main script")

test_that("Default arguments", {
  ## Defaults:
  opts <- remake_main_parse_args(character(0))
  expect_that(opts$options$remake_file, equals("remake.yml"))
  expect_that(opts$options$quiet, equals(FALSE))
  expect_that(opts$options$list_targets, equals(FALSE))
  expect_that(opts$options$script, equals(FALSE))
  expect_that(opts$options$script_file, equals(NULL))
  expect_that(opts$options$version, equals(FALSE))
  expect_that(opts$options$help, equals(FALSE))
  expect_that(opts$args, equals(character(0)))

  ## No unknown options:
  known_opts <- c("help", "list_targets", "quiet", "remake_file", 
                  "script", "version")  
  expect_that(sort(names(opts$options)),
              equals(sort(known_opts)))
})

test_that("Misspelt arguments", {
  expect_that(remake_main_parse_args("--quit"),
              throws_error('Invalid targets: "--quit"'))
  expect_that(remake_main_parse_args(c("--foo", "--bar", "baz")),
              throws_error('Invalid targets: "--foo", "--bar"'))
})

test_that("Version", {
  expect_that(main("--version"),
              shows_message(paste0("remake version ",
                                   packageVersion("remake"))))
})

test_that("List targets", {
  cmp <- list_targets()
  expect_that(main("--list-targets"),
              shows_message(paste(cmp, collapse="\n")))
})

test_that("Plain", {
  cleanup()
  expect_that(file.exists("plot.pdf"), is_false())
  expect_that(main(character(0)),
              shows_message("[ ----- ] all", fixed=TRUE))
  expect_that(file.exists("plot.pdf"), is_true())
  ## Quiet:
  expect_that(main("-q"), not(shows_message()))
})

test_that("Multiple targets", {
  cleanup()
  dat <- evaluate_promise(main(c("processed", "data.csv")))
  ## TODO: This would be heaps easier if we logged what we did
  ## somewhere.
  expected <- c(
    "[  LOAD ]",
    "[  READ ]",
    "<  MAKE > processed",
    "[ BUILD ] data.csv",
    "[ BUILD ] processed",
    "<  MAKE > data.csv",
    "[    OK ] data.csv")
  expect_that(length(dat$messages), equals(length(expected)))
  expect_that(substr(dat$messages, 1, nchar(expected)),
              equals(expected))
})

test_that("Different remake file", {
  cmp <- list_targets("chain.yml")
  expect_that(main(c("--list-targets", "-f", "chain.yml")),
              shows_message(paste(cmp, collapse="\n")))
  ## This style of option parsing does not work:
  ## expect_that(main(c("--list-targets", "-fchain.yml")),
  ##             shows_message(paste(cmp, collapse="\n")))
  expect_that(main(c("--list-targets", "--file=chain.yml")),
              shows_message(paste(cmp, collapse="\n")))
  expect_that(main(c("--list-targets", "--file", "chain.yml")),
              shows_message(paste(cmp, collapse="\n")))
})

test_that("Script", {
  cleanup()
  expect_that(main(c("-s")),
              shows_message("LOAD"))
  ## With quiet:
  expect_that(main(c("-s", "-q")),
              not(shows_message("READ")))
  expect_that(capture.output(main(c("-s", "-q"))),
              equals(make_script()))
  tmp <- tempfile()
  main(c("-s", "-q", "--script-file", tmp))
  expect_that(readLines(tmp), equals(make_script()))
})

test_that("Installation", {
  cleanup()
  expect_that(file.exists("remake"), is_false())
  install_remake(".")
  expect_that(file.exists("remake"), is_true())
  ## TODO: I don't know why this is not working; it works fine
  ## interactively.  It looks like when running non-interactively
  ## we're not setting TERM to a dumb type so colours are getting
  ## mixed in.
  str <- system2("./remake", stdout=TRUE, stderr=TRUE)  
  if (interactive()) {
    expect_that(any(grepl("[ BUILD ] plot.pdf", str, fixed=TRUE)),
                is_true())
  }

  expect_that(install_remake("."), throws_error("already exists"))
  expect_that(install_remake(".", TRUE),
              not(throws_error()))

  ## Using internal access:
  expect_that("remake:::main()" %in% readLines("remake"),
              is_true())
  ## Working around methods:
  expect_that("library(methods)" %in% readLines("remake"),
              is_true())
})

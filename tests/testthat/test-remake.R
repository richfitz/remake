context("Loading remakefiles")

test_that("Recursive remakefiles", {
  expect_error(remake("recursive1.yml"),
               "Recursive include detected")
  expect_error(remake("recursive2.yml"),
               "Recursive include detected")
  expect_error(remake("recursive3.yml"),
               "Recursive include detected")
})

test_that("Target/rule clash", {
  cleanup()
  expect_warning(m <- remake("remake_rule_clash.yml"),
                 "Rule name clashes with target name")
  remake_make(m)
  expect_true(file.exists("plot.pdf"))
})

test_that("Quoting", {
  template <- '
sources:
  - code.R

targets:
  data.csv:
    command: download_data(%s)
    cleanup_level: purge

  processed:
    command: process_data(%s)

  plot.pdf:
    command: myplot(%s)
    plot: true
'
  ## First the OK version:
  filename <- "tmp_quoting.yml"
  on.exit(file_remove(filename))

  writeLines(sprintf(template, 'target_name', '"data.csv"', 'processed'),
             filename)
  expect_error(remake(filename), NA)

  writeLines(sprintf(template, '"data.csv"', '"data.csv"', 'processed'),
             filename)
  expect_error(remake(filename), NA)

  ## Then different errors:
  ## The first two are *useless* errors
  writeLines(sprintf(template, '"target_name"', '"data.csv"', 'processed'),
             filename)
  expect_error(remake(filename),
               "target_name must not be quoted")

  writeLines(sprintf(template, 'data.csv', '"data.csv"', 'processed'),
             filename)
  expect_error(remake(filename),
               "target name must be quoted")

  writeLines(sprintf(template, 'target_name', 'data.csv', 'processed'),
             filename)
  expect_error(remake(filename),
               "Incorrect quotation in target")
  expect_error(remake(filename),
               "Should be quoted: data.csv")

  writeLines(sprintf(template, 'target_name', '"data.csv"', '"processed"'),
             filename)
  expect_error(remake(filename),
               "Incorrect quotation in target")
  expect_error(remake(filename),
               "Should not be quoted: processed")
})

test_that("Verbosity", {
  default <- remake_verbose()
  expect_true(default$print_progress)
  expect_true(default$print_noop)
  expect_true(default$print_command)
  expect_true(default$print_command_abbreviate)
  expect_null(default$quiet_target)

  quiet <- remake_verbose(FALSE)
  expect_false(quiet$print_progress)
  expect_true(quiet$print_noop)
  expect_true(quiet$print_command)
  expect_true(quiet$print_command_abbreviate)
  expect_null(quiet$quiet_target)

  x <- remake_verbose(default)
  expect_identical(x, default)

  expect_error(remake_verbose(1), "verbose must be logical")
})

test_that("Caching", {
  cleanup()
  expect_null(cache$fetch("remake.yml"))
  m <- remake("remake.yml")
  expect_equal(cache$fetch("remake.yml"), m)
  cleanup()
  expect_null(cache$fetch("remake.yml"))
})

test_that("Loading without sources", {
  cleanup()
  ## No caching here:
  msg <- capture_messages(m <- remake_new(load_sources=FALSE))
  expect_false(any(grepl("loading sources", msg)))
  expect_message(m <- remake_new(load_sources=TRUE), "loading sources")

  ## With caching:
  cleanup()
  msg <- capture_messages(m <- remake(load_sources=FALSE))
  expect_false(any(grepl("loading sources", msg)))
  expect_is(m$store$env, "managed_environment")
  expect_null(m$store$env$env)
  ## This means that things *will* get cleared
  expect_false(m$store$env$is_current())

  ## And again from the cache no message:
  expect_message(m <- remake(load_sources=FALSE), NA)

  ## Check again:
  m <- cache$fetch("remake.yml")
  expect_is(m, "remake")
  expect_null(m$store$env$env)

  ## This does load the sources
  expect_message(m <- remake(), "loading sources")
  expect_is(m$store$env$env, "environment")
  ## Sources already loaded
  msg <- capture_messages(m <- remake())
  expect_false(any(grepl("loading sources", msg)))
})

test_that("Missing targets", {
  cleanup()
  filename <- "tmp_missing_targets.yml"
  dat <- list(targets=
                list(a_target=
                       list(command="download_data(another_target)"),
                     another_target=
                       list(command="f()")))
  writeLines(yaml::as.yaml(dat), filename)
  on.exit(file_remove(filename))
  ## Sanity check!
  expect_error(remake(filename), NA)

  ## Misspelling:
  dat$targets$a_target$command <- "download_data(aother_target)"
  writeLines(yaml::as.yaml(dat), filename)
  expect_error(remake(filename),
               "created targets must all be files")
  str <- "aother_target: (in a_target) -- did you mean: another_target"
  expect_error(remake(filename), str, fixed=TRUE)

  ## Warning if it looks like a file.
  dat$targets$a_target$command <- "download_data('an/other_target')"
  writeLines(yaml::as.yaml(dat), filename)
  expect_warning(remake(filename),
                 "Creating implicit target for nonexistant files")
  str <- "an/other_target: (in a_target) -- did you mean: another_target"
  expect_warning(remake(filename, allow_cache=FALSE), str, fixed=TRUE)

  ## No suggestions:
  oo <- options(remake.dym=FALSE)
  on.exit(options(oo), add=TRUE)
  expect_warning(remake(filename, allow_cache=FALSE),
                 "Creating implicit target for nonexistant files")
})

test_that("Source case sensitivity", {
  expect_error(remake("remake_wrong_case.yml"), "Files not found")
  if (is_case_insensitive()) {
    expect_error(remake("remake_wrong_case.yml"),
                 "incorrect case => code.R")
  }
})

test_that("ggplot targets", {
  ## I don't want to depend on ggplot2 for this dependency, so it's
  ## conditional:
  if (suppressWarnings(require(ggplot2, quietly=TRUE))) {
    make(remake_file="ggplot.yml")
    expect_equal(hash_files("plot_manual.png", FALSE),
                 hash_files("plot_auto.png", FALSE))
    file.remove("plot_manual.png")
    file.remove("plot_auto.png")
  }
})

## Courtesy of Andrew:
test_that("Function dependencies", {
  code <- c('foo <- function() "A"',
            'bar <- function() foo()')
  filename <- "dependencies.R"
  writeLines(code, filename)
  on.exit(file.remove(filename))

  res <- make("all", remake_file="remake_dependncies.yml")
  expect_equal(res, "A")
  expect_true(is_current("all", remake_file="remake_dependncies.yml"))

  code[[1]] <- sub("A", "B", code[[1]])
  writeLines(code, filename)

  expect_false(is_current("all", remake_file="remake_dependncies.yml"))
  res <- make("all", remake_file="remake_dependncies.yml")
  expect_equal(res, "B")
})

test_that("file target must make files", {
  expect_error(remake::make(remake_file="remake_file_missing.yml"),
               "command for plot2.png did not create file")
})

test_that("custom extensions", {
  file_remove("data.phy")
  file_remove("plot.pdf")
  res <- make("all", remake_file="remake_extension.yml")
  expect_true(file.exists("data.phy"))
  expect_true(file.exists("plot.pdf"))
})

test_that("Return values for multiple targets", {
  cleanup()
  ret_data <- make("data.csv")
  ret_processed <- make("processed")

  cleanup()
  ret <- make(c("processed", "data.csv"))
  expect_equal(ret, ret_data)
  ret <- make(c("processed", "data.csv"))
  expect_equal(ret, ret_data)

  cleanup()
  ret <- make(c("data.csv", "processed"))
  expect_equal(ret, ret_processed)
  ret <- make(c("data.csv", "processed"))
  expect_equal(ret, ret_processed)
})

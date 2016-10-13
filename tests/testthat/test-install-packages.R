context("Install packages")

## This will get fleshed out over time, but doing this without
## creating a horrible pile of dependencies is tricky.  I notice that
## devtools doesn't actually test installation.
##
## Update: this is crazy hard to test right.
##
## TODO: pull out in favour of context I think.

test_that("sources", {
  dat <- read_remake_packages("remake_sources.yml")
  expect_equal(dat$sowsear$repo, "richfitz/sowsear")
})

test_that("install_packages", {
  extras <- read_remake_packages("remake_sources.yml")
  cmp_devtools_cran <- 'install.packages("devtools")'
  expect_equal(install_packages("devtools", TRUE, FALSE, extras),
               cmp_devtools_cran)
  expect_equal(install_packages("sowsear", TRUE, FALSE, extras),
               c(cmp_devtools_cran,
                 'devtools::install_github("richfitz/sowsear")'))

  ## Here's a weird case that allows bootstrapping of devtools:
  extras <- list(devtools=list(source="github", repo="hadley/devtools"))
  expect_equal(install_packages("devtools", TRUE, FALSE, extras),
               c(cmp_devtools_cran,
                 'devtools::install_github("hadley/devtools")'))

  ## Pick a package that does not exist:
  extras <- list(notreal=list(source="github", repo="notreal/notreal"))
  expect_equal(install_packages("notreal", TRUE, TRUE, extras),
               'devtools::install_github("notreal/notreal")')
})

## This test is going to be too slow to do most of the time.  So
## probably best to
test_that("install_packages (for reals)", {
  skip_unless_set("REMAKE_TEST_INSTALL_PACKAGES")
  if ("sowsear" %in% .packages(TRUE)) {
    remove.packages("sowsear", .libPaths())
  }

  ## Then try actually running this:
  extras <- read_remake_packages("remake_sources.yml")
  install_packages("sowsear", instructions=TRUE, package_sources=extras)
  res <- install_packages("sowsear", package_sources=extras)
  expect_equal(res, "sowsear")
  expect_true("sowsear" %in% .packages(TRUE))

  res <- install_packages("sowsear", package_sources=extras)
  expect_equal(res, character(0))
})

test_that("loading a remakefile with a missing package", {
  cleanup()
  obj <- suppressWarnings(remake("remake_missing_package.yml"))
  expect_error(.remake_initialize_packages(obj),
               'install.packages("nosuchpackage")', fixed=TRUE)

  skip_unless_set("REMAKE_TEST_INSTALL_PACKAGES")
  expect_warning(m <- remake("remake_missing_package.yml", allow_cache=FALSE),
                 "Some packages are missing: nosuchpackage")
  expect_error(.remake_initialize_packages(m),
               "Some packages are missing")

  ## TODO: There is some disagreement here about the error.  I see
  ##   there is no package called ‘nosuchpackage’
  ## sometimes and
  ##   ‘nosuchpackage’ is not available
  ## others.  One looks like a library error ('there is no package')
  ## which means that the things that should have thrown didn't throw?
  ## Or didn't catch.
  ##
  ## I don't see the cause here (nor do I care enough to seriously
  ## track it down - we're doing approximately the right thing).
  msg <- "(is not available|there is no package called)"
  with_options(list(remake.install.missing.packages=TRUE),
               expect_error(
                 suppressWarnings(.remake_initialize_packages(m), msg)))
})

test_that("Allow missing package on load", {
  cleanup()
  expect_warning(obj <- remake("remake_missing_package.yml",
                               allow_missing_packages=TRUE), NA)
  expect_error(.remake_initialize_packages(obj), NA)
  ## Not cached; so we still get warning/errors here:
  expect_warning(obj2 <- remake("remake_missing_package.yml"),
                 "Some packages are missing: nosuchpackage")
  expect_error(.remake_initialize_packages(obj2),
               "Some packages are missing")
  expect_warning(obj3 <- remake("remake_missing_package.yml",
                                allow_missing_packages=TRUE), NA)
  ## Caching has left these in the correct position:
  expect_false(obj2$allow_missing_packages)
  expect_true(obj3$allow_missing_packages)

  ## Not tested: how the missing packages thing deals with
  ## installation.  Just consider it to be undefined behaviour for
  ## now.

  ## Then we can build the target
  expect_is(remake_make(obj, "processed"), "data.frame")
  ## And of course the version that does require the package to load
  ## will see that the database contains the built object already.
  expect_is(remake_make(obj2, "processed"), "data.frame")
})

test_that("loading a remakefile with a missing package", {
  skip_unless_set("REMAKE_TEST_INSTALL_PACKAGES")
  if ("sowsear" %in% .packages(TRUE)) {
    remove.packages("sowsear", .libPaths())
  }

  expect_warning(m <- remake("remake_missing_sowsear.yml"),
                 "Some packages are missing: sowsear")
  expect_error(.remake_initialize_packages(m),
               "devtools::install_github")

  oo <- options(remake.install.missing.packages=TRUE)
  on.exit(options(oo))
  expect_message(.remake_initialize_packages(m),
                 "Downloading GitHub repo", ignore.case=TRUE)
  expect_true("sowsear" %in% .packages())
  unload_extra_packages("sowsear")
})

test_that("loading a remakefile with a missing target-specific package", {
  skip_unless_set("REMAKE_TEST_INSTALL_PACKAGES")
  cleanup()
  if ("sowsear" %in% .packages(TRUE)) {
    remove.packages("sowsear", .libPaths())
  }

  m <- remake("remake_missing_sowsear2.yml", allow_cache=FALSE)
  expect_warning(.remake_initialize_packages(m),
                 "devtools::install_github")
  with_options(list(remake.warn.missing.target.packages=FALSE),
               expect_warning(.remake_initialize_packages(m), NA))

  expect_error(remake_make(m, "data.csv"), NA)

  expect_error(remake_make(m, "processed"),
               "devtools::install_github")
  expect_false(remake_is_current(m, "processed"))

  ## TODO: This seems not to work because of a *devtools* issue,
  ## probably because I removed the package during a session. Either
  ## way it's highly annoying and means I'll probably cut the devtools
  ## link soon.
  skip("devtools is broken")
  msg <- capture_messages(with_options(
    list(remake.install.missing.packages=TRUE),
    remake_make(m, "processed")))
  expect_match(msg, "Installing missing required packages:")
  expect_true(remake_is_current(m, "processed"))
  expect_false("sowsear" %in% .packages())
})

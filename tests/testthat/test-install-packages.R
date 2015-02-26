context("Install packages")

## This will get fleshed out over time, but doing this without
## creating a horrible pile of dependencies is tricky.  I notice that
## devtools doesn't actually test installation.
##
## Update: this is crazy hard to test right.

test_that("sources", {
  dat <- read_remake_packages("remake_sources.yml")
  expect_that(dat$sowsear$repo, equals("richfitz/sowsear"))
})

test_that("install_packages", {
  extras <- read_remake_packages("remake_sources.yml")
  cmp_devtools_cran <- 'install.packages("devtools")'
  expect_that(install_packages("devtools", TRUE, FALSE, extras),
              equals(cmp_devtools_cran))
  expect_that(install_packages("sowsear", TRUE, FALSE, extras),
              equals(c(cmp_devtools_cran,
                       'devtools::install_github("richfitz/sowsear")')))

  ## Here's a weird case that allows bootstrapping of devtools:
  extras <- list(devtools=list(source="github", repo="hadley/devtools"))
  expect_that(install_packages("devtools", TRUE, FALSE, extras),
              equals(c(cmp_devtools_cran,
                       'devtools::install_github("hadley/devtools")')))

  ## Pick a package that does not exist:
  extras <- list(notreal=list(source="github", repo="notreal/notreal"))
  expect_that(install_packages("notreal", TRUE, TRUE, extras),
              equals('devtools::install_github("notreal/notreal")'))
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
  expect_that(res, equals("sowsear"))
  expect_that("sowsear" %in% .packages(TRUE), is_true())

  res <- install_packages("sowsear", package_sources=extras)
  expect_that(res, equals(character(0)))
})

test_that("loading a remakefile with a missing package", {
  cleanup()
  obj <- remake("remake_missing_package.yml")
  expect_that(.remake_initialize_packages(obj),
              throws_error('install.packages("nosuchpackage")', fixed=TRUE))

  skip_unless_set("REMAKE_TEST_INSTALL_PACKAGES")
  expect_that(m <- remake("remake_missing_package.yml", allow_cache=FALSE),
              gives_warning("Some packages are missing: nosuchpackage"))
  expect_that(.remake_initialize_packages(m),
              throws_error("Some packages are missing"))

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
  msg <- (if (interactive()) "is not available"
          else                "there is no package called")
  with_options(list(remake.install.missing.packages=TRUE),
               expect_that(.remake_initialize_packages(m),
                           throws_error(msg)))
})

test_that("loading a remakefile with a missing package", {
  skip_unless_set("REMAKE_TEST_INSTALL_PACKAGES")
  if ("sowsear" %in% .packages(TRUE)) {
    remove.packages("sowsear", .libPaths())
  }

  expect_that(m <- remake("remake_missing_sowsear.yml"),
              gives_warning("Some packages are missing: sowsear"))
  expect_that(.remake_initialize_packages(m),
              throws_error("devtools::install_github"))

  oo <- options(remake.install.missing.packages=TRUE)
  on.exit(options(oo))
  expect_that(.remake_initialize_packages(m),
              shows_message("Downloading github repo"))
  expect_that("sowsear" %in% .packages(), is_true())
  unload_extra_packages("sowsear")
})

test_that("loading a remakefile with a missing target-specific package", {
  skip_unless_set("REMAKE_TEST_INSTALL_PACKAGES")
  cleanup()
  if ("sowsear" %in% .packages(TRUE)) {
    remove.packages("sowsear", .libPaths())
  }

  m <- remake("remake_missing_sowsear2.yml", allow_cache=FALSE)
  expect_that(.remake_initialize_packages(m),
              gives_warning("devtools::install_github"))
  with_options(list(remake.warn.missing.target.packages=FALSE),
               expect_that(.remake_initialize_packages(m),
                           not(gives_warning())))

  expect_that(remake_make(m, "data.csv"),
              not(throws_error()))

  expect_that(remake_make(m, "processed"),
              throws_error("devtools::install_github"))
  expect_that(remake_is_current(m, "processed"), is_false())

  with_options(list(remake.install.missing.packages=TRUE),
               expect_that(remake_make(m, "processed"),
                           shows_message("Downloading github repo")))
  expect_that(remake_is_current(m, "processed"), is_true())
  expect_that("sowsear" %in% .packages(), is_false())
})

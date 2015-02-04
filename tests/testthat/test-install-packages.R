if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Package sources")

## This will get fleshed out over time, but doing this without
## creating a horrible pile of dependencies is tricky.  I notice that
## devtools doesn't actually test installation.
test_that("sources", {
  dat <- read_maker_packages("maker_sources.yml")
  expect_that(dat$sowsear$repo, equals("richfitz/sowsear"))
})

test_that("install_packages", {
  extras <- read_maker_packages("maker_sources.yml")
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
  skip_unless_travis()
  set_cran_mirror()
  if ("sowsear" %in% .packages(TRUE)) {
    remove.packages("sowsear", .libPaths())
  }

  path <- tempfile()
  dir.create(path)
  .libPaths(path)

  ## Then try actually running this:
  extras <- read_maker_packages("maker_sources.yml")
  install_packages("sowsear", instructions=TRUE, package_sources=extras)
  res <- install_packages("sowsear", package_sources=extras)
  expect_that(res, equals("sowsear"))
  expect_that(dir(path), equals("sowsear"))

  res <- install_packages("sowsear", package_sources=extras)
  expect_that(res, equals(character(0)))

  unlink(path, recursive=TRUE)
  .libPaths(path)
})

test_that("loading a makerfile with a missing package", {
  expect_that(maker("maker_missing_package.yml"),
              throws_error('install.packages("nosuchpackage")', fixed=TRUE))

  ## skip_unless_travis()
  ## set_cran_mirror()
  ## oo <- options(maker.install.missing.packages=TRUE)
  ## on.exit(options(oo))
  ##
  ## TODO: This actually only errors after dropping *back* into maker;
  ## should turn the warning it gives into an error I think.
  ##
  ## TODO: I can't actually test this, oddly; somehow the tryCatch
  ## doesn't nest or something?
  ## expect_that(maker("maker_missing_package.yml"),
  ##             throws_error("is not available"))
})

test_that("loading a makerfile with a missing package", {
  skip_unless_travis()
  set_cran_mirror()
  if ("sowsear" %in% .packages(TRUE)) {
    remove.packages("sowsear", .libPaths())
  }
  path <- tempfile()
  dir.create(path)
  .libPaths(path)

  expect_that(m <- maker("maker_missing_sowsear.yml"),
              throws_error("devtools::install_github"))
  oo <- options(maker.install.missing.packages=TRUE)
  on.exit(options(oo))
  expect_that(m <- maker("maker_missing_sowsear.yml"),
              shows_message("Downloading github repo"))
  expect_that(m, is_a("maker"))
  expect_that("sowsear" %in% .packages(), is_true())

  unlink(path, recursive=TRUE)
  .libPaths(path)
})

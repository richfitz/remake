if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
}

context("Code dependencies")

## Hashing environments is going to basically not work as it will
## *always* change, so we have to deparse the function to get the
## *representation* of the function.  This has the nice side effect of
## dropping the comments, and normalising differences in
## whitespace/indenting, so that we're insensitive to changes that
## will not affect the code.

## We *are* sensitive to changes in things like the global environment
## though.  It's possible we could use codetools::findGlobals to
## identify things that *might* be global, then to check the global
## environment for these and depend on things there?

## The basic idea here is that we'll restrict everything to just the
## environment that we look after.  Check all functions there, build
## the dependency tree and hash those functions.  We'll only compute
## dependencies for functions in this environment.

## Ideally though we'd compute information on the *packages* that
## everything else depends on.  That's moderately easy to do, and can
## be added in the future.  Basically, where at the moment we have the
## test:
##      identical(environment(r), env)
## have the `else` clause of that try to get the packageName of
## a function.

## This approach will fall over for the case where we depend on
## generated functions, on objects with reference semantics, etc, but
## those are going to be difficult to check.  The only problem there
## is that people will have to manually rebuild those targets.  The
## same thing will happen with things like database connections that
## we can't see.  A decent solution would be to flag such targets as
## `always_rebuild`, perhaps.

## This does not recurse, and getting it to do so will be entertaining...
test_that("Simple case", {
  e <- new.env()
  sys.source("deps.R", e)

  deps <- code_deps$new(e)

  ## TODO: does not give error with unknown function
  expect_that(deps$depends_functions("bottom"), equals("bottom"))
  expect_that(deps$depends_functions("single"),
              equals(c("bottom", "single")))
  expect_that(deps$depends_functions("double"),
              equals(c("bottom", "double", "single")))
  expect_that(deps$depends_functions("self"), equals("self"))

  expect_that(deps$depends_packages("bottom"), equals("base"))
  expect_that(deps$depends_packages("single"), equals("base"))
  expect_that(deps$depends_packages("double"), equals("base"))
  expect_that(deps$depends_packages("self"),   equals(character(0)))

  cmp <- list(functions=list(
                bottom=hash_function(e$bottom),
                single=hash_function(e$single)),
              packages=list(
                base=as.character(packageVersion("base"))))
  expect_that(deps$info("single"), equals(cmp))

  expect_that(deps$depends_functions("nonexistant"),
              equals(character(0)))
  empty <- structure(list(), names=character(0))
  expect_that(deps$info("nonexistant"),
              equals(list(functions=empty, packages=empty)))
})

test_that("deparse tricks", {
  f <- function() {
    0.0001
  }

  ## First check the R behaviour that triggers the bug:
  oo <- options(scipen=0)
  on.exit(options(oo))
  expect_that(deparse(f)[[3]], matches("\\s*1e-04$"))
  h1 <- digest::digest(deparse(f))
  options(scipen=1000)
  expect_that(deparse(f)[[3]], matches("0\\.0001$"))
  h2 <- digest::digest(deparse(f))
  expect_that(h1, not(equals(h2)))

  ## But we don't see this:
  expect_that(hash_function(f), equals(h1))
  options(scipen=0)
  expect_that(hash_function(f), equals(h1))
})

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

  deps <- code_deps(e)
  deps_names <- function(x) {
    names(deps(x)$functions)
  }

  ## TODO: does not give error with unknown function
  expect_that(deps_names("bottom"), equals("bottom"))
  expect_that(deps_names("single"),
              equals(c("bottom", "single")))
  expect_that(deps_names("double"),
              equals(c("bottom", "double", "single")))
  expect_that(deps_names("self"), equals("self"))

  cmp <- list(functions=list(
                bottom=hash_function(e$bottom),
                single=hash_function(e$single)))
  expect_that(deps("single"), equals(cmp))

  expect_that(deps_names("nonexistant"),
              equals(character(0)))
  expect_that(deps("nonexistant"),
              equals(list(functions=empty_named_list())))
})

test_that("deparse scipen", {
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

test_that("deparse cutoff", {
  ## Just check that deparsing does not seem to respond to the cutoff
  ## option though:
  g <- function() {
    very + very + long + line + that + r + will + want + to + split + somewhere
  }
  oo <- options(deparse.cutoff=50)
  on.exit(options(oo))
  s1 <- deparse(g)

  options(deparse.cutoff=60)
  s2 <- deparse(g)
  expect_that(s1, equals(s2))

  ## Bug here with equals:
  s3 <- deparse(g, width.cutoff=50)
  expect_that(s1, not(is_identical_to(s3)))
})

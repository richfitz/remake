if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Parse commands")

test_that("Corner cases", {
  expect_that(parse_command(character(0)),
              throws_error("str must be a scalar"))
  expect_that(parse_command(NULL),
              throws_error("str must be a scalar"))
  expect_that(parse_command(c("foo(1)", "foo(2)")),
              throws_error("str must be a scalar"))
  expect_that(parse_command("foo(1); foo(2)"),
              throws_error("Expected single expression"))
  expect_that(parse_command("foo"),
              throws_error("Expected a function call"))

  expect_that(parse_command("1()"),
              throws_error("Rule must be a character or name"))

  ## But this is OK (but debatably).  This is because things like
  ## 'sin'(1) are OK.  You can even do
  ##   `1` <- sin
  ##   '1'(1)
  ## which is just plain evil.
  expect_that(parse_command("'1'()"), not(throws_error()))

  ## This is also OK, and that's not great:
  expect_that(parse_command("{}"), not(throws_error()))
  ## This does fail though:
  expect_that(parse_command("()"), throws_error("Error in parse"))

  expect_that(parse_command("foo(1)"),
              throws_error("Not yet handled"))
  expect_that(parse_command("foo(TRUE)"),
              throws_error("Not yet handled"))
})

test_that("Rules with no arguments", {
  res <- parse_command("foo()")
  expect_that(res$rule, equals("foo"))
  expect_that(res$args, equals(list()))
  expect_that(res$depends, equals(empty_named_integer()))
})

test_that("Rules with one argument", {
  res <- parse_command("foo(a)")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(quote(a))))
  expect_that(res$depends, equals(c(a=1L)))

  res <- parse_command("foo(argname=a)")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(argname=quote(a))))
  expect_that(res$depends, equals(c(a=1)))

  res <- parse_command("foo('a')")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list("a")))
  expect_that(res$depends, equals(c(a=1L)))

  res <- parse_command("foo(argname='a')")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(argname="a")))
  expect_that(res$depends, equals(c(a=1L)))

  expect_that(parse_command("foo(1)"),
              throws_error("Not yet handled"))
  expect_that(parse_command("foo(argname=1)"),
              throws_error("Not yet handled"))
  expect_that(parse_command("foo(TRUE)"),
              throws_error("Not yet handled"))
  expect_that(parse_command("foo(argname=FALSE)"),
              throws_error("Not yet handled"))
})

test_that("Rules with multiple arguments", {
  res <- parse_command("foo(a, b)")
  expect_that(res$rule, equals("foo"))
  expect_that(res$args, equals(list(quote(a), quote(b))))
  expect_that(res$depends, equals(c(a=1L, b=2L)))

  res <- parse_command("foo(arg1=a, b)")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(arg1=quote(a), quote(b))))
  expect_that(res$depends, equals(c(a=1L, b=2L)))

  res <- parse_command("foo(a, arg2=b)")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(quote(a), arg2=quote(b))))
  expect_that(res$depends, equals(c(a=1L, b=2L)))

  res <- parse_command("foo(arg1=a, arg2=b)")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(arg1=quote(a), arg2=quote(b))))
  expect_that(res$depends, equals(c(a=1L, b=2L)))

  ## Quote one of these:
  res <- parse_command("foo(a, 'b')")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(quote(a), "b")))
  expect_that(res$depends, equals(c(a=1L, b=2L)))

  res <- parse_command("foo(arg1=a, 'b')")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(arg1=quote(a), "b")))
  expect_that(res$depends, equals(c(a=1L, b=2L)))

  res <- parse_command("foo(a, arg2='b')")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(quote(a), arg2="b")))
  expect_that(res$depends, equals(c(a=1L, b=2L)))

  res <- parse_command("foo(arg1=a, arg2='b')")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(arg1=quote(a), arg2="b")))
  expect_that(res$depends, equals(c(a=1L, b=2L)))
})

test_that("target_argument detection", {
  expect_that(parse_target_command("a", "foo()"),
              equals(list(rule="foo",
                          args=list(),
                          depends=empty_named_integer())))
  cmp_pos <- list(rule="foo",
                  args=list("a"),
                  depends=empty_named_integer())
  expect_that(parse_target_command("a", "foo('a')"), equals(cmp_pos))
  expect_that(parse_target_command("a", "foo(target_name)"),
              equals(cmp_pos))

  cmp_name <- list(rule="foo",
                  args=list(arg="a"),
                  depends=empty_named_integer())
  expect_that(parse_target_command("a", "foo(arg='a')"), equals(cmp_name))
  expect_that(parse_target_command("a", "foo(arg=target_name)"),
              equals(cmp_name))

  ## These should error:
  expect_that(parse_target_command("a", "foo(a)"),
              throws_error("target name must be quoted"))
  expect_that(parse_target_command("a", "foo('target_name')"),
              throws_error("target_name must not be quoted"))
  expect_that(parse_target_command("a", "foo(arg=a)"),
              throws_error("target name must be quoted"))
  expect_that(parse_target_command("a", "foo(arg='target_name')"),
              throws_error("target_name must not be quoted"))

  f <- function(a, x) parse_target_command(a, x)$depends

  expect_that(f("a", "foo('a', b)"),     equals(c(b=2L)))
  expect_that(f("a", "foo(b, 'a')"),     equals(c(b=1L)))
  expect_that(f("a", "foo(arg='a', b)"), equals(c(b=2L)))
  expect_that(f("a", "foo(b, arg='a')"), equals(c(b=1L)))

  expect_that(f("a", "foo(target_name, b)"),     equals(c(b=2L)))
  expect_that(f("a", "foo(b, target_name)"),     equals(c(b=1L)))
  expect_that(f("a", "foo(arg=target_name, b)"), equals(c(b=2L)))
  expect_that(f("a", "foo(b, arg=target_name)"), equals(c(b=1L)))

  ## Errors:
  expect_that(parse_target_command("a", "foo(a, a)"),
              throws_error("multiple times"))
  expect_that(parse_target_command("a", "foo(target_name, target_name)"),
              throws_error("multiple times"))
  expect_that(parse_target_command("a", "foo(a, target_name)"),
              throws_error("multiple times"))
})

## TODO: Add things that have non-symbol objects within them.  Things
## like list(a, b), for example.

test_that("chain", {
  cmd <- parse_target_command("a", c("foo(x)", "bar(.)"))
  expect_that(cmd$rule,    equals("bar")) # last element of chain
  expect_that(cmd$args,    equals(list(quote(.))))
  expect_that(cmd$depends, equals(c("."=1L)))
  expect_that(length(cmd$chain), equals(1L))
  expect_that(cmd$chain[[1]]$rule,    equals("foo"))
  expect_that(cmd$chain[[1]]$args,    equals(list(quote(x))))
  expect_that(cmd$chain[[1]]$depends, equals(c(x=1L)))

  cmd <- parse_target_command("x", c("foo(a, b)", "bar(c, .)"))
  expect_that(cmd$chain[[1]]$args, equals(list(quote(a), quote(b))))
  expect_that(cmd$args, equals(list(quote(c), quote(.))))

  cmd <- parse_target_command("x", c("foo(a, b)", "bar(arg1=c, arg2=.)"))
  expect_that(cmd$args, equals(list(arg1=quote(c), arg2=quote(.))))

  expect_that(parse_target_command("a", c("foo()", "bar('.')")),
              throws_error("Dot argument must not be quoted"))

  expect_that(parse_target_command("a", c("foo(.)", "bar(.)")),
              throws_error("The first element in a chain cannot contain a dot"))
  expect_that(parse_target_command("a", c("foo()", "bar(x)")),
              throws_error("All chain elements except the first need a dot"))

  expect_that(parse_target_command("a", c("foo()", "bar(., .)")),
              throws_error("Only a single dot argument allowed"))
})

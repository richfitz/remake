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
})

test_that("Rules with no arguments", {
  res <- parse_command("foo()")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list()))
})

test_that("Rules with one argument", {
  res <- parse_command("foo(a)")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list("a")))
  expect_that(res$quoted, is_false())

  res <- parse_command("foo(argname=a)")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list(argname="a")))
  expect_that(res$quoted, is_false())

  res <- parse_command("foo('a')")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list("a")))
  expect_that(res$quoted, is_true())

  res <- parse_command("foo(argname='a')")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list(argname="a")))
  expect_that(res$quoted, is_true())

  expect_that(parse_command("foo(1)"),
              throws_error("Every element must be a character or name"))
  expect_that(parse_command("foo(argname=1)"),
              throws_error("Every element must be a character or name"))
  expect_that(parse_command("foo(TRUE)"),
              throws_error("Every element must be a character or name"))
  expect_that(parse_command("foo(argname=FALSE)"),
              throws_error("Every element must be a character or name"))
})

test_that("Rules with multiple arguments", {
  res <- parse_command("foo(a, b)")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list("a", "b")))
  expect_that(res$quoted, equals(c(FALSE, FALSE)))

  res <- parse_command("foo(arg1=a, b)")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list(arg1="a", "b")))
  expect_that(res$quoted, equals(c(FALSE, FALSE)))

  res <- parse_command("foo(a, arg2=b)")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list("a", arg2="b")))
  expect_that(res$quoted, equals(c(FALSE, FALSE)))

  res <- parse_command("foo(arg1=a, arg2=b)")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list(arg1="a", arg2="b")))
  expect_that(res$quoted, equals(c(FALSE, FALSE)))

  ## Quote one of these:
  res <- parse_command("foo(a, 'b')")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list("a", "b")))
  expect_that(res$quoted, equals(c(FALSE, TRUE)))

  res <- parse_command("foo(arg1=a, 'b')")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list(arg1="a", "b")))
  expect_that(res$quoted, equals(c(FALSE, TRUE)))

  res <- parse_command("foo(a, arg2='b')")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list("a", arg2="b")))
  expect_that(res$quoted, equals(c(FALSE, TRUE)))

  res <- parse_command("foo(arg1=a, arg2='b')")
  expect_that(res$rule, equals("foo"))
  expect_that(res$depends, equals(list(arg1="a", arg2="b")))
  expect_that(res$quoted, equals(c(FALSE, TRUE)))
})

test_that("target_argument detection", {
  expect_that(parse_target_command("a", "foo()"),
              equals(list(rule="foo", depends=list(), quoted=logical(0))))

  cmp_pos  <- list(rule="foo", depends=list(), quoted=logical(0),
                   target_argument=1)
  expect_that(parse_target_command("a", "foo('a')"), equals(cmp_pos))
  expect_that(parse_target_command("a", "foo(target_name)"),
              equals(cmp_pos))

  cmp_name <- list(rule="foo", depends=empty_named_list(), quoted=logical(0),
                   target_argument="arg")
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

  f <- function(a, x) parse_target_command(a, x)$target_argument

  expect_that(f("a", "foo('a', b)"),     equals(1))
  expect_that(f("a", "foo(b, 'a')"),     equals(2))
  expect_that(f("a", "foo(arg='a', b)"), equals("arg"))
  expect_that(f("a", "foo(arg='a', b)"), equals("arg"))

  expect_that(f("a", "foo(target_name, b)"),     equals(1))
  expect_that(f("a", "foo(b, target_name)"),     equals(2))
  expect_that(f("a", "foo(arg=target_name, b)"), equals("arg"))
  expect_that(f("a", "foo(arg=target_name, b)"), equals("arg"))

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
  cmd <- parse_target_chain("a", c("foo(x)", "bar(.)"))
  expect_that(cmd$rule, equals("bar")) # last element of chain
  expect_that(cmd$depends, equals(list(".")))
  expect_that(cmd$quoted, equals(FALSE))
  expect_that(length(cmd$chain), equals(1L))
  expect_that(cmd$chain[[1]]$rule, equals("foo"))
  expect_that(cmd$chain[[1]]$depends, equals(list("x")))
  expect_that(cmd$chain[[1]]$quoted, equals(FALSE))
})

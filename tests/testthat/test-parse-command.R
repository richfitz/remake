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

  res <- parse_command("foo(1)")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(1L)))
  expect_that(res$depends, equals(empty_named_integer()))
  expect_that(res$is_target, equals(FALSE))

  res <- parse_command("foo(argname=1)")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(argname=1L)))
  expect_that(res$depends, equals(empty_named_integer()))
  expect_that(res$is_target, equals(FALSE))

  res <- parse_command("foo(TRUE)")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(TRUE)))
  expect_that(res$depends, equals(empty_named_integer()))
  expect_that(res$is_target, equals(FALSE))

  res <- parse_command("foo(argname=TRUE)")
  expect_that(res$rule,    equals("foo"))
  expect_that(res$args,    equals(list(argname=TRUE)))
  expect_that(res$depends, equals(empty_named_integer()))
  expect_that(res$is_target, equals(FALSE))
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
                          depends=empty_named_integer(),
                          is_target=logical(0),
                          command=quote(foo()))))

  cmp_pos <- list(rule="foo",
                  args=list("a"),
                  depends=empty_named_integer(),
                  is_target=FALSE,
                  command=quote(foo("a")))
  expect_that(parse_target_command("a", "foo('a')"), equals(cmp_pos))
  cmp_pos$command <- quote(foo(target_name))
  expect_that(parse_target_command("a", "foo(target_name)"),
              equals(cmp_pos))

  cmp_name <- list(rule="foo",
                   args=list(arg="a"),
                   depends=empty_named_integer(),
                   is_target=FALSE,
                   command=quote(foo(arg="a")))
  expect_that(parse_target_command("a", "foo(arg='a')"), equals(cmp_name))
  cmp_name$command <- quote(foo(arg=target_name))
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

  expect_that(parse_target_command("a", "foo('a')")$is_target,
              equals(FALSE))
  expect_that(parse_target_command("a", "foo('a', 'b')")$is_target,
              equals(c(FALSE, TRUE)))

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

test_that("Literal arguments", {
  obj <- parse_command("foo(target_arg, I(literal_arg))")
  expect_that(obj$args[[2]], equals(quote(literal_arg)))

  expect_that(parse_command("foo(target_arg, TRUE)")$args[[2]],
              is_identical_to(TRUE))
  expect_that(parse_command("foo(target_arg, 1L)")$args[[2]],
              is_identical_to(1L))
  expect_that(parse_command("foo(target_arg, 1.0)")$args[[2]],
              is_identical_to(1.0))
  expect_that(parse_command("foo(target_arg, 1i)")$args[[2]],
              is_identical_to(1i))
})

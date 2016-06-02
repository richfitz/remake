context("Parse commands")

test_that("Corner cases", {
  expect_error(parse_command(character(0)),
               "str must be a scalar")
  expect_error(parse_command(NULL),
               "str must be a scalar")
  expect_error(parse_command(c("foo(1)", "foo(2)")),
               "str must be a scalar")
  expect_error(parse_command("foo(1); foo(2)"),
               "Expected single expression")
  expect_error(parse_command("foo"),
               "Expected a function call")

  expect_error(parse_command("1()"),
               "Rule must be a character or name")

  ## But this is OK (but debatably).  This is because things like
  ## 'sin'(1) are OK.  You can even do
  ##   `1` <- sin
  ##   '1'(1)
  ## which is just plain evil.
  expect_error(parse_command("'1'()"), NA)

  ## This is also OK, and that's not great:
  expect_error(parse_command("{}"), NA)
  ## This does fail though:
  msg <- tryCatch(parse(text="()"), error=function(e) e)$message
  expect_error(parse_command("()"), msg, fixed=TRUE)
})

test_that("Rules with no arguments", {
  res <- parse_command("foo()")
  expect_equal(res$rule, "foo")
  expect_equal(res$args, list())
  expect_equal(res$depends, character(0))
})

test_that("Rules with one argument", {
  res <- parse_command("foo(a)")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(quote(a)))
  expect_equal(res$depends, "a")

  res <- parse_command("foo(argname=a)")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(argname=quote(a)))
  expect_equal(res$depends, c(argname="a"))

  res <- parse_command("foo('a')")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list("a"))
  expect_equal(res$depends, "a")

  res <- parse_command("foo(argname='a')")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(argname="a"))
  expect_equal(res$depends, c(argname="a"))

  res <- parse_command("foo(1)")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(1L))
  expect_equal(res$depends, character(0))
  expect_equal(res$is_target, FALSE)

  res <- parse_command("foo(argname=1)")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(argname=1L))
  expect_equal(res$depends, empty_named_character())
  expect_equal(res$is_target, FALSE)

  res <- parse_command("foo(TRUE)")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(TRUE))
  expect_equal(res$depends, character(0))
  expect_equal(res$is_target, FALSE)

  res <- parse_command("foo(argname=TRUE)")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(argname=TRUE))
  expect_equal(res$depends, empty_named_character())
  expect_equal(res$is_target, FALSE)
})

test_that("Rules with multiple arguments", {
  res <- parse_command("foo(a, b)")
  expect_equal(res$rule, "foo")
  expect_equal(res$args, list(quote(a), quote(b)))
  expect_equal(res$depends, c("a", "b"))

  res <- parse_command("foo(arg1=a, b)")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(arg1=quote(a), quote(b)))
  expect_equal(res$depends, c(arg1="a", "b"))

  res <- parse_command("foo(a, arg2=b)")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(quote(a), arg2=quote(b)))
  expect_equal(res$depends, c("a", arg2="b"))

  res <- parse_command("foo(arg1=a, arg2=b)")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(arg1=quote(a), arg2=quote(b)))
  expect_equal(res$depends, c(arg1="a", arg2="b"))

  ## Quote one of these:
  res <- parse_command("foo(a, 'b')")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(quote(a), "b"))
  expect_equal(res$depends, c("a", "b"))

  res <- parse_command("foo(arg1=a, 'b')")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(arg1=quote(a), "b"))
  expect_equal(res$depends, c(arg1="a", "b"))

  res <- parse_command("foo(a, arg2='b')")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(quote(a), arg2="b"))
  expect_equal(res$depends, c("a", arg2="b"))

  res <- parse_command("foo(arg1=a, arg2='b')")
  expect_equal(res$rule,    "foo")
  expect_equal(res$args,    list(arg1=quote(a), arg2="b"))
  expect_equal(res$depends, c(arg1="a", arg2="b"))
})

test_that("target_argument detection", {
  expect_equal(parse_target_command("a", "foo()"),
               list(rule="foo",
                    args=list(),
                    depends=character(0),
                    is_target=logical(0),
                    command=quote(foo())))

  cmp_pos <- list(rule="foo",
                  args=list("a"),
                  depends=character(0),
                  is_target=FALSE,
                  command=quote(foo("a")))
  expect_equal(parse_target_command("a", "foo('a')"), cmp_pos)
  expect_equal(parse_target_command("a", "foo(target_name)"), cmp_pos)

  cmp_name <- list(rule="foo",
                   args=list(arg="a"),
                   depends=empty_named_character(),
                   is_target=FALSE,
                   command=quote(foo(arg="a")))
  expect_equal(parse_target_command("a", "foo(arg='a')"), cmp_name)
  expect_equal(parse_target_command("a", "foo(arg=target_name)"), cmp_name)

  ## These should error:
  expect_error(parse_target_command("a", "foo(a)"),
               "target name must be quoted")
  expect_error(parse_target_command("a", "foo('target_name')"),
               "target_name must not be quoted")
  expect_error(parse_target_command("a", "foo(arg=a)"),
               "target name must be quoted")
  expect_error(parse_target_command("a", "foo(arg='target_name')"),
               "target_name must not be quoted")

  f <- function(a, x) parse_target_command(a, x)$depends


  named_b <- structure("b", names="")
  expect_equal(f("a", "foo('a', b)"),     "b")
  expect_equal(f("a", "foo(b, 'a')"),     "b")
  expect_equal(f("a", "foo(arg='a', b)"), named_b)
  expect_equal(f("a", "foo(b, arg='a')"), named_b)

  expect_equal(f("a", "foo(target_name, b)"),     "b")
  expect_equal(f("a", "foo(b, target_name)"),     "b")
  expect_equal(f("a", "foo(arg=target_name, b)"), named_b)
  expect_equal(f("a", "foo(b, arg=target_name)"), named_b)

  expect_equal(parse_target_command("a", "foo('a')")$is_target,
               FALSE)
  expect_equal(parse_target_command("a", "foo('a', 'b')")$is_target,
               c(FALSE, TRUE))

  ## Errors:
  expect_error(parse_target_command("a", "foo(a, a)"),
               "multiple times")
  expect_error(parse_target_command("a", "foo(target_name, target_name)"),
               "multiple times")
  expect_error(parse_target_command("a", "foo(a, target_name)"),
               "multiple times")
})

## TODO: Add things that have non-symbol objects within them.  Things
## like list(a, b), for example.

test_that("Literal arguments", {
  obj <- parse_command("foo(target_arg, I(literal_arg))")
  expect_equal(obj$args[[2]], quote(literal_arg))

  expect_identical(parse_command("foo(target_arg, TRUE)")$args[[2]], TRUE)
  expect_identical(parse_command("foo(target_arg, 1L)")$args[[2]], 1L)
  expect_identical(parse_command("foo(target_arg, 1.0)")$args[[2]], 1.0)
  expect_identical(parse_command("foo(target_arg, 1i)")$args[[2]], 1i)
})

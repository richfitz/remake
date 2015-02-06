if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-remake.R")
}

context("Interactive remake")

test_that("Drop braces", {
  expect_that(interactive_drop_braces(quote(a)),
              is_identical_to(quote(a)))
  expect_that(interactive_drop_braces(quote({a})),
              is_identical_to(quote(a)))
  expect_that(interactive_drop_braces(quote({{a}})),
              is_identical_to(quote(a)))

  expect_that(interactive_drop_braces(quote(a <- b)),
              is_identical_to(quote(a <- b)))
  expect_that(interactive_drop_braces(quote({a <- b})),
              is_identical_to(quote(a <- b)))
  expect_that(interactive_drop_braces(quote({{a <- b}})),
              is_identical_to(quote(a <- b)))

  ## Not sure why not identical here:
  expect_that(interactive_drop_braces(quote({a <- {b}})),
              equals(quote(a <- {b})))
  expect_that(interactive_drop_braces(quote({a <- {b; c}})),
              equals(quote(a <- {b; c})))

  expect_that(interactive_drop_braces(quote({a; b})),
              throws_error("Expected non-compound expression"))
  expect_that(interactive_drop_braces(quote({a; b; c})),
              throws_error("Expected non-compound expression"))

  expect_that(interactive_drop_braces(quote({})),
              equals(quote({})))
})

test_that("add targets", {
  m <- remake(NULL)
  expect_that(names(m$interactive$targets), equals(character(0)))
  expect_that(m$active, is_false())
  expect_that(m$interactive$targets, is_identical_to(empty_named_list()))

  m$add <- target("a", foo(b))
  expect_that(names(m$interactive$targets), equals("a"))
  expect_that(m$interactive$targets$a, is_a("target_object"))

  expect_that(m$interactive$targets$a,
              equals(make_target("a", list(command=quote(foo(b))))))
  expect_that(m$interactive$targets$a,
              equals(make_target("a", list(command="foo(b)"))))

  m$add <- target("p.pdf", foo(b), plot=TRUE)

  cmp <- make_target("p.pdf", list(command=quote(foo(b)), plot=TRUE))
  expect_that(m$interactive$targets$p.pdf, equals(cmp))
})

test_that("add sources", {
  m <- remake(NULL)
  expect_that(m$interactive$sources, equals(character(0)))

  m$add <- character(0)
  expect_that(m$interactive$sources, equals(character(0)))

  m$add <- c("file1.R", "file2.R")
  expect_that(m$interactive$sources, equals(c("file1.R", "file2.R")))

  ## Adding again does nothing:
  m$add <- c("file1.R", "file2.R")
  expect_that(m$interactive$sources, equals(c("file1.R", "file2.R")))

  m$add <- c("file3.R", "file4.R")
  expect_that(m$interactive$sources, equals(sprintf("file%d.R", 1:4)))
})

test_that("all together (Active)", {
  cleanup()
  m <- remake(NULL)
  expect_that(inherits(m, "remake_interactive"), is_true())

  m <- remake(NULL)
  m$add <- "package:testthat"
  m$add <- "code.R"
  m$add <- target("data.csv", download_data(target_name),
                  cleanup_level="purge")
  m$add <- target("processed", process_data("data.csv"))
  m$add <- target("plot.pdf", myplot(processed),
                  plot=list(width=8, height=4))

  expect_that(m$active, is_false())
  m$make("plot.pdf")
  expect_that(m$active, is_true())

  expect_that(file.exists("plot.pdf"), is_true())
})

test_that("Active and global", {
  cleanup()

  e <- new.env()
  m <- remake(NULL, envir=e)
  expect_that(inherits(m, "remake_interactive"), is_true())
  expect_that(remake_active_bindings(m$m), not(is_null()))
  expect_that(m$m$store, not(is_null()))
  expect_that(m$m$store$env, not(is_null()))
  expect_that(m$m$store$env, is_a("managed_environment"))

  m$add <- "package:testthat"
  m$add <- "code.R"

  expect_that("download_data" %in% ls(e), is_true())
  expect_that(bindingIsActive("download_data", e), is_true())
  expect_that(is.function(e$download_data), is_true())

  ## Next, add targets:
  m$add <- target("data.csv", download_data(target_name),
                  cleanup_level="purge")
  expect_that(exists("data.csv", e), is_false())

  m$add <- target("processed", process_data("data.csv"))
  expect_that(exists("processed", e), is_true())
  expect_that(bindingIsActive("processed", e), is_true())

  ## This is the broken bit now:
  expect_that(e$processed, is_a("target_placeholder"))
  expect_that(print(e$processed),
              prints_text("<target processed>"))

  m$add <- target("plot.pdf", myplot(processed),
                  plot=list(width=8, height=4))

  expect_that(remake_active_bindings(m$m)$bindings$target,
              equals("processed"))

  expect_that(m$active, is_false())
  m$make("plot.pdf")
  expect_that(m$active, is_true())

  expect_that(file.exists("plot.pdf"), is_true())
  expect_that(bindingIsActive("processed", e), is_true())
  expect_that(e$processed, is_a("data.frame"))
})

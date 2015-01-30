if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Interactive maker")

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

test_that("add_target", {
  m <- maker(NULL)
  expect_that(names(m$interactive$targets), equals(character(0)))
  expect_that(m$interactive$targets, is_identical_to(empty_named_list()))

  add_target(m, "a", foo(b))
  expect_that(names(m$interactive$targets), equals("a"))
  expect_that(m$interactive$targets$a, is_a("target_object"))

  expect_that(m$interactive$targets$a,
              equals(make_target("a", list(command=quote(foo(b))))))
  expect_that(m$interactive$targets$a,
              equals(make_target("a", list(command="foo(b)"))))

  add_target(m, "p.pdf", foo(b), plot=TRUE)

  cmp <- make_target("p.pdf", list(command=quote(foo(b)), plot=TRUE))
  expect_that(m$interactive$targets$p.pdf, equals(cmp))
})

test_that("add_sources", {
  m <- maker(NULL)
  expect_that(m$interactive$sources, equals(character(0)))

  add_sources(m, character(0))
  expect_that(m$interactive$sources, equals(character(0)))

  add_sources(m, "file1.R", "file2.R")
  expect_that(m$interactive$sources, equals(c("file1.R", "file2.R")))

  ## Adding again does nothing:
  add_sources(m, "file1.R", "file2.R")
  expect_that(m$interactive$sources, equals(c("file1.R", "file2.R")))

  add_sources(m, c("file3.R", "file4.R"))
  expect_that(m$interactive$sources, equals(sprintf("file%d.R", 1:4)))
})

test_that("all together", {
  cleanup()
  m <- maker(NULL)
  expect_that(m$is_interactive(), is_true())
  add_sources(m, "code.R")
  add_packages(m, "testthat")
  add_target(m, "data.csv", download_data(target_name),
             cleanup_level="purge")
  add_target(m, "processed", process_data("data.csv"))
  add_target(m, "plot.pdf", myplot(processed),
             plot=list(width=8, height=4))

  m$make("plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())
})

test_that("all together (Active)", {
  cleanup()
  m <- maker(NULL)
  expect_that(m$is_interactive(), is_true())

  m <- maker(NULL)
  m$add <- "package:testthat"
  m$add <- "code.R"
  m$add <- target("data.csv", download_data(target_name),
                  cleanup_level="purge")
  m$add <- target("processed", process_data("data.csv"))
  m$add <- target("plot.pdf", myplot(processed),
                  plot=list(width=8, height=4))

  m$make("plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())
})

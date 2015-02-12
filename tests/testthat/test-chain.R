context("Chained rules")

test_that("Chained rules", {
  cleanup()
  m <- remake("chain.yml")

  res <- remake_make(m, "manual")
  expect_that(res, equals(6))

  ## There are two chain rules created:
  expect_that(c("chained{1}", "chained{2}") %in%
              list_targets(remake_file="chain.yml"),
              equals(c(FALSE, FALSE)))
  expect_that(c("chained{1}", "chained{2}") %in%
              list_targets(remake_file="chain.yml",
                           include_chain_intermediates=TRUE),
              equals(c(TRUE, TRUE)))

  t <- m$targets[["chained"]]

  t1 <- m$targets[["chained{1}"]]
  expect_that(t1, is_a("target_base"))
  expect_that(t1, is_a("target_object"))
  expect_that(t1$chain_parent$name, equals(t$name))
  expect_that(t1$depends_name, equals(character(0)))

  t2 <- m$targets[["chained{2}"]]
  expect_that(t2, is_a("target_base"))
  expect_that(t2, is_a("target_object"))
  expect_that(t2$chain_parent$name, equals(t$name))
  expect_that(t2$depends_name, equals(t1$name))
  expect_that(unname(t2$depends_type), equals("object"))

  expect_that(length(t$chain_kids), equals(2))

  res <- remake_make(m, "chained")
  expect_that(res, equals(6))

  expect_that(m$store$objects$contains("chained"),    is_true())
  expect_that(m$store$objects$contains("chained{1}"), is_true())
  expect_that(m$store$objects$contains("chained{2}"), is_true())

  ## Cleaning removes them all:
  remake_make(m, "clean")
  expect_that(m$store$objects$contains("chained"), is_false())
  expect_that(m$store$objects$contains("chained{1}"), is_false())
  expect_that(m$store$objects$contains("chained{2}"), is_false())

  ## By default, so does remove_target:
  res <- remake_make(m, "chained")
  remake_remove_target(m, "chained")
  expect_that(m$store$objects$contains("chained"), is_false())
  expect_that(m$store$objects$contains("chained{1}"), is_false())
  expect_that(m$store$objects$contains("chained{2}"), is_false())

  res <- remake_make(m, "chained")
  remake_remove_target(m, "chained", FALSE)
  expect_that(m$store$objects$contains("chained"), is_false())
  expect_that(m$store$objects$contains("chained{1}"), is_true())
  expect_that(m$store$objects$contains("chained{2}"), is_true())
})

test_that("Chained rules -> file", {
  cleanup()
  m <- remake("chain_file.yml")
  remake_make(m, "data.csv")
  remake_make(m, "plot.pdf")

  expect_that(file.exists("plot.pdf"), is_true())
  expect_that(m$store$objects$contains("plot.pdf{1}"), is_true())

  ## This can't currently be tested, but the intermediate object won't
  ## be rebuilt.
  file.remove("plot.pdf")
  remake_make(m, "plot.pdf")

  remake_make(m, "clean")
  expect_that(file.exists("plot.pdf"), is_false())
  expect_that(m$store$objects$contains("plot.pdf{1}"), is_false())

  remake_make(m, "plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())
  expect_that(m$store$objects$contains("plot.pdf{1}"), is_true())
  remake_remove_target(m, "plot.pdf")
  expect_that(file.exists("plot.pdf"), is_false())
  expect_that(m$store$objects$contains("plot.pdf{1}"), is_false())
})

test_that("Chained rules -> plot", {
  cleanup()
  m <- remake("chain_plot.yml")
  remake_make(m, "data.csv")
  remake_make(m, "plot.pdf")

  expect_that(file.exists("plot.pdf"), is_true())
  expect_that(m$store$objects$contains("plot.pdf{1}"), is_true())

  ## This can't currently be tested, but the intermediate object won't
  ## be rebuilt.
  file.remove("plot.pdf")
  remake_make(m, "plot.pdf")

  remake_make(m, "clean")
  expect_that(file.exists("plot.pdf"), is_false())
  expect_that(m$store$objects$contains("plot.pdf{1}"), is_false())

  remake_make(m, "plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())
  expect_that(m$store$objects$contains("plot.pdf{1}"), is_true())
  remake_remove_target(m, "plot.pdf")
  expect_that(file.exists("plot.pdf"), is_false())
  expect_that(m$store$objects$contains("plot.pdf{1}"), is_false())
})

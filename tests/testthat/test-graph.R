context("Graph")

test_that("Simple order", {
  graph <- list(b=NULL,
                e="k",
                k=c("b", "i", "j"),
                g=c("k", "h"),
                i=NULL,
                j=NULL,
                h="i",
                c=c("j", "h"))
  order <- topological_order(graph)
  expect_equal(order,
              c("b", "i", "j", "k", "e", "h", "g", "c"))
  expect_equal(topological_sort(graph), graph[order])
})

test_that("Cycle detection", {
  expect_error(topological_order(list(a="a")),
               "cyclic dependency")
  expect_error(topological_order(list(a="b", b="c", c="a")),
               "cyclic dependency")
})

test_that("Dependency filtering", {
  graph <- list(a="b", b="c", c="d", d=NULL)
  expect_equal(dependencies("a", graph), letters[1:4])
  expect_equal(dependencies("b", graph), letters[2:4])
  expect_equal(dependencies("c", graph), letters[3:4])
  expect_equal(dependencies("d", graph), "d")
})

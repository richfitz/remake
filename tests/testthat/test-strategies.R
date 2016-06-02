context("Build strategies")

test_that("Plan", {
  ## Harvested from remake -- may resurrect at some point:
  m_status <- function(m, target_name=NULL) {
    if (is.null(target_name)) {
      target_name <- remake_default_target(m)
    }
    remake_status(m, target_name, remake_dependency_graph(m))
  }

  cleanup()
  m <- remake()
  p <- remake_plan(m, "plot.pdf")

  cmp <- c("data.csv", "processed", "plot.pdf")
  expect_equal(remake_plan(m, "plot.pdf"), cmp)

  ## Default plan:
  expect_equal(remake_plan(m), c(cmp, "all"))

  cmp_status <- cbind(dirty=rep(TRUE, 3),
                      dirty_by_descent=c(FALSE, TRUE, TRUE))
  rownames(cmp_status) <- cmp

  expect_equal(m_status(m, "plot.pdf"), cmp_status)
  expect_equal(m_status(m), rbind(cmp_status, all=c(TRUE, TRUE)))

  ## Now, build the data:
  remake_make(m, "data.csv")
  cmp_status["data.csv","dirty"] <- FALSE
  cmp_status["processed","dirty_by_descent"] <- FALSE
  expect_equal(m_status(m, "plot.pdf"), cmp_status)

  remake_make(m, "processed")
  cmp_status["processed","dirty"] <- FALSE
  cmp_status["plot.pdf","dirty_by_descent"] <- FALSE
  expect_equal(m_status(m, "plot.pdf"), cmp_status)

  remake_make(m, "plot.pdf")
  cmp_status["plot.pdf","dirty"] <- FALSE
  expect_equal(m_status(m, "plot.pdf"), cmp_status)
  expect_equal(m_status(m), rbind(cmp_status, all=c(TRUE, FALSE)))

  ## Now, delete data.csv: Notice that the plot.pdf is not actually
  ## dirty, though it *is* dirty by descent.
  file_remove("data.csv")
  cmp_status[c("data.csv", "processed"), "dirty"] <- TRUE
  cmp_status[c("processed", "plot.pdf"), "dirty_by_descent"] <- TRUE
  expect_equal(m_status(m, "plot.pdf"), cmp_status)
  expect_equal(m_status(m), rbind(cmp_status, all=c(TRUE, TRUE)))
})

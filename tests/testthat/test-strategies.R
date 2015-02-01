if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Build strategies")

test_that("Plan", {
  ## Harvested from maker -- may resurrect at some point:
  m_status <- function(m, target_name=NULL) {
    private <- maker_private(m)
    if (is.null(target_name)) {
      target_name <- private$target_default()
    }
    status(target_name, private$dependency_graph(), m)
  }

  cleanup()
  m <- maker()
  p <- maker_private(m)$plan("plot.pdf")

  cmp <- c("data.csv", "processed", "plot.pdf")
  expect_that(maker_private(m)$plan("plot.pdf"), equals(cmp))

  ## Default plan:
  expect_that(maker_private(m)$plan(), equals(c(cmp, "all")))

  cmp_status <- cbind(dirty=rep(TRUE, 3),
                      dirty_by_descent=c(FALSE, TRUE, TRUE))
  rownames(cmp_status) <- cmp

  expect_that(m_status(m, "plot.pdf"), equals(cmp_status))
  expect_that(m_status(m), equals(rbind(cmp_status, all=c(TRUE, TRUE))))

  ## Now, build the data:
  m$make("data.csv")
  cmp_status["data.csv","dirty"] <- FALSE
  cmp_status["processed","dirty_by_descent"] <- FALSE
  expect_that(m_status(m, "plot.pdf"), equals(cmp_status))

  m$make("processed")
  cmp_status["processed","dirty"] <- FALSE
  cmp_status["plot.pdf","dirty_by_descent"] <- FALSE
  expect_that(m_status(m, "plot.pdf"), equals(cmp_status))

  m$make("plot.pdf")
  cmp_status["plot.pdf","dirty"] <- FALSE
  expect_that(m_status(m, "plot.pdf"), equals(cmp_status))
  expect_that(m_status(m), equals(rbind(cmp_status, all=c(TRUE, FALSE))))

  ## Now, delete data.csv: Notice that the plot.pdf is not actually
  ## dirty, though it *is* dirty by descent.
  file.remove("data.csv")
  cmp_status[c("data.csv", "processed"), "dirty"] <- TRUE
  cmp_status[c("processed", "plot.pdf"), "dirty_by_descent"] <- TRUE
  expect_that(m_status(m, "plot.pdf"), equals(cmp_status))
  expect_that(m_status(m), equals(rbind(cmp_status, all=c(TRUE, TRUE))))
})

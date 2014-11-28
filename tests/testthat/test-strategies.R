if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Build strategies")

test_that("Plan", {
  cleanup()
  m <- maker()
  p <- m$plan("plot.pdf")

  cmp <- c("data.csv", "processed", "plot.pdf")
  expect_that(m$plan("plot.pdf"), equals(cmp))

  ## Default plan:
  expect_that(m$plan(), equals(c(cmp, "all")))

  cmp_status <- cbind(dirty=rep(TRUE, 3),
                      dirty_by_descent=c(FALSE, TRUE, TRUE))
  rownames(cmp_status) <- cmp
  expect_that(m$status("plot.pdf"), equals(cmp_status))
  expect_that(m$status(), equals(rbind(cmp_status, all=c(TRUE, TRUE))))

  ## Now, build the data:
  m$make("data.csv")
  cmp_status["data.csv","dirty"] <- FALSE
  cmp_status["processed","dirty_by_descent"] <- FALSE
  expect_that(m$status("plot.pdf"), equals(cmp_status))

  m$make("processed")
  cmp_status["processed","dirty"] <- FALSE
  cmp_status["plot.pdf","dirty_by_descent"] <- FALSE
  expect_that(m$status("plot.pdf"), equals(cmp_status))

  m$make("plot.pdf")
  cmp_status["plot.pdf","dirty"] <- FALSE
  expect_that(m$status("plot.pdf"), equals(cmp_status))
  expect_that(m$status(), equals(rbind(cmp_status, all=c(TRUE, FALSE))))

  ## Now, delete data.csv: Notice that the plot.pdf is not actually
  ## dirty, though it *is* dirty by descent.
  file.remove("data.csv")
  cmp_status[c("data.csv", "processed"), "dirty"] <- TRUE
  cmp_status[c("processed", "plot.pdf"), "dirty_by_descent"] <- TRUE
  expect_that(m$status("plot.pdf"), equals(cmp_status))
  expect_that(m$status(), equals(rbind(cmp_status, all=c(TRUE, TRUE))))
})

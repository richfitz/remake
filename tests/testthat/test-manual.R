if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
}

context("Manual run")

cleanup <- function() {
  unlink(".maker", recursive=TRUE)
  suppressWarnings(file.remove(c("data.csv", "plot.pdf")))
  invisible(NULL)
}

test_that("simple run", {
  cleanup()
  m <- maker$new("config.yml")

  expect_that(m$is_current("data.csv"), is_false())
  expect_that(m$is_current("processed"), is_false())
  expect_that(m$is_current("plot.pdf"), is_false())

  ## This is possibly overkill:
  expect_that(m$dependency_status("data.csv", TRUE),
              equals(list(name="data.csv", depends=list())))
  expect_that(m$dependency_status("processed", TRUE),
              equals(list(name="processed",
                          depends=list(list(
                            name="data.csv",
                            type="file",
                            hash=NA_character_)))))
  expect_that(m$dependency_status("plot.pdf", TRUE),
              equals(list(name="plot.pdf",
                          depends=list(list(
                            name="processed",
                            type="object",
                            hash=NA_character_)))))

  ## Run the build system manually:
  m$build("data.csv")
  m$build("processed")
  m$build("plot.pdf")

  expect_that(m$is_current("data.csv"),  is_true())
  expect_that(m$is_current("processed"), is_true())
  expect_that(m$is_current("plot.pdf"),  is_true())

  cleanup()
})

test_that("Depending on a file we don't make", {
  cleanup()
  ## Manually run the download step from before -- now we have a file
  ## that maker wants to depend on, but does not generate:
  e <- new.env()
  source("code.R", e)
  e$download_data("data.csv")
  ## This configuration is the same as config.yml, but it does not
  ## contain a rule for building data.csv
  m <- maker$new("config2.yml")

  expect_that(m$build("data.csv"),
              throws_error("Can't build implicit targets"))
  m$build("processed")
  m$build("plot.pdf")

  cleanup()
})

test_that("Fake targets", {
  cleanup()
  m <- maker$new("config.yml")
  expect_that(m$is_current("data.csv"), is_false())
  expect_that(m$is_current("processed"), is_false())
  expect_that(m$is_current("plot.pdf"), is_false())
  m$make("everything")
  expect_that(m$is_current("data.csv"),  is_true())
  expect_that(m$is_current("processed"), is_true())
  expect_that(m$is_current("plot.pdf"),  is_true())
  cleanup()
})

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

## TODO: Should be able to depend on *files* that aren't targets.  I
## think at this point that is not possible, but that's going to be
## the case for external files.  However, depending on external
## *objects* is never OK.

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

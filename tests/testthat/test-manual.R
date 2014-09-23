if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Manual run")

test_that("simple run", {
  cleanup()
  m <- maker$new("config.yml")

  expect_that(m$is_current("data.csv"), is_false())
  expect_that(m$is_current("processed"), is_false())
  expect_that(m$is_current("plot.pdf"), is_false())

  ## This is possibly overkill:
  ## Hmm, this is *wrong*.  Should depend on all sorts of things that
  ## generated data.csv?
  cmp <- list(name="data.csv",
              depends=list(),
              code=list(
                functions=list(download_data=hash_function(
                                 m$env$download_data)),
                packages=list(utils=as.character(
                                packageVersion("utils")))))
  expect_that(m$dependency_status("data.csv", TRUE), equals(cmp))

  cmp <- list(name="processed",
              depends=list(
                list(name="data.csv", type="file", hash=NA_character_)),
              code=list(
                functions=list(process_data=hash_function(
                                 m$env$process_data)),
                packages=list(utils=as.character(
                                packageVersion("utils")))))
  expect_that(m$dependency_status("processed", TRUE), equals(cmp))

  res <- m$dependency_status("plot.pdf", TRUE)

  ## TODO:: This horrible section gets simpler after we get code for
  ## comparing different json output without being sensitive to
  ## ordering.
  pkgs <- c("grDevices", "graphics")
  expect_that(sort(names(res$code$packages)), equals(sort(pkgs)))
  res$code$packages <- res$code$packages[pkgs]

  fns <- c("do_plot", "myplot")
  expect_that(sort(names(res$code$functions)), equals(sort(fns)))
  res$code$functions <- res$code$functions[fns]

  cmp <- list(name="plot.pdf",
              depends=list(
                list(name="processed", type="object", hash=NA_character_)),
              code=list(
                functions=list(
                  do_plot=hash_function(m$env$do_plot),
                  myplot=hash_function(m$env$myplot)),
                packages=list(
                  grDevices=as.character(packageVersion("grDevices")),
                  graphics=as.character(packageVersion("graphics"))
                  )))
  expect_that(res, equals(cmp))

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

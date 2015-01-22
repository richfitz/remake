if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
  source("helper-maker.R")
}

context("Manual run")

test_that("simple run", {
  cleanup()
  m <- maker("maker.yml")
  m$load_sources()

  expect_that(m$is_current("data.csv"), is_false())
  expect_that(m$is_current("processed"), is_false())
  expect_that(m$is_current("plot.pdf"), is_false())

  ## This is possibly overkill:
  cmp <- list(version=m$store$version,
              name="data.csv",
              depends=empty_named_list(),
              fixed=hash_object(list("data.csv")),
              code=list(
                functions=list(download_data=hash_function(
                                 m$store$env$env$download_data)),
                packages=list(utils=as.character(
                                packageVersion("utils")))))
  expect_that(m$dependency_status("data.csv", TRUE), equals(cmp))

  cmp <- list(version=m$store$version,
              name="processed",
              depends=list("data.csv"=NA_character_),
              fixed=NULL,
              code=list(
                functions=list(process_data=hash_function(
                                 m$store$env$env$process_data)),
                packages=list(utils=as.character(
                                packageVersion("utils")))))
  expect_that(m$dependency_status("processed", TRUE), equals(cmp))

  ## NOTE: I don't really understand why we're sensitive to order
  ## here, differently from the command line and within R.  It seems
  ## to be how grDevices/graphics sort on lower/uppercase -- setting
  ## different locales perhaps?  This is dealt with by the
  ## identical_map() function within the package
  res <- m$dependency_status("plot.pdf", TRUE)
  pkgs <- c("grDevices", "graphics")
  expect_that(sort(names(res$code$packages)), equals(sort(pkgs)))
  res$code$packages <- res$code$packages[pkgs]
  cmp <- list(version=m$store$version,
              name="plot.pdf",
              depends=list(processed=NA_character_),
              fixed=hash_object(list("plot.pdf")),
              code=list(
                functions=list(
                  do_plot=hash_function(m$store$env$env$do_plot),
                  myplot=hash_function(m$store$env$env$myplot)),
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
  expect_that(file.exists("data.csv"), is_true())

  ## This configuration is the same as maker.yml, but it does not
  ## contain a rule for building data.csv
  m <- maker("maker2.yml")
  m$load_sources()

  expect_that(m$build("data.csv"),
              throws_error("Can't build implicit targets"))
  m$build("processed")
  m$build("plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())
  m$make("clean")
  expect_that(file.exists("plot.pdf"), is_false())
  expect_that(file.exists("data.csv"), is_true())

  cleanup()
})

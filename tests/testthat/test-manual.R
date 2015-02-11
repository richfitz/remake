context("Manual run")

test_that("simple run", {
  cleanup()
  m <- remake("remake.yml")

  expect_that(is_current("data.csv", m), is_false())
  expect_that(is_current("processed", m), is_false())
  expect_that(is_current("plot.pdf", m), is_false())

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
  expect_that(dependency_status(m$targets[["data.csv"]], m$store, TRUE),
              equals(cmp))

  cmp <- list(version=m$store$version,
              name="processed",
              depends=list("data.csv"=NA_character_),
              fixed=NULL,
              code=list(
                functions=list(process_data=hash_function(
                                 m$store$env$env$process_data)),
                packages=list(utils=as.character(
                                packageVersion("utils")))))
  expect_that(dependency_status(m$targets[["processed"]], m$store, TRUE),
              equals(cmp))

  ## NOTE: I don't really understand why we're sensitive to order
  ## here, differently from the command line and within R.  It seems
  ## to be how grDevices/graphics sort on lower/uppercase -- setting
  ## different locales perhaps?  This is dealt with by the
  ## identical_map() function within the package
  res <- dependency_status(m$targets[["plot.pdf"]], m$store, TRUE)
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
  remake_update(m, "data.csv", force=TRUE)
  remake_update(m, "processed", force=TRUE)
  remake_update(m, "plot.pdf", force=TRUE)

  expect_that(is_current("data.csv", m),  is_true())
  expect_that(is_current("processed", m), is_true())
  expect_that(is_current("plot.pdf", m),  is_true())

  cleanup()
})

test_that("Depending on a file we don't make", {
  cleanup()
  ## Manually run the download step from before -- now we have a file
  ## that remake wants to depend on, but does not generate:
  e <- new.env()
  source("code.R", e)
  e$download_data("data.csv")
  expect_that(file.exists("data.csv"), is_true())

  ## This configuration is the same as remake.yml, but it does not
  ## contain a rule for building data.csv
  m <- remake("remake2.yml")

  expect_that(remake_update(m, "data.csv", force=TRUE),
              throws_error("Can't build implicit targets"))
  remake_update(m, "processed", force=TRUE)
  remake_update(m, "plot.pdf", force=TRUE)
  expect_that(file.exists("plot.pdf"), is_true())
  remake_make(m, "clean")
  expect_that(file.exists("plot.pdf"), is_false())
  expect_that(file.exists("data.csv"), is_true())

  cleanup()
})

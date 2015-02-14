context("Manual run")

test_that("simple run", {
  cleanup()
  m <- remake("remake.yml")

  expect_that(remake_is_current(m, "data.csv"), is_false())
  expect_that(remake_is_current(m, "processed"), is_false())
  expect_that(remake_is_current(m, "plot.pdf"), is_false())

  ## This is possibly overkill:
  cmp <- list(version=m$store$version,
              name="data.csv",
              type="file",
              depends=empty_named_list(),
              fixed=hash_object(list("data.csv")),
              code=list(
                functions=list(download_data=hash_function(
                                 m$store$env$env$download_data)),
                packages=list(utils=as.character(
                                packageVersion("utils")))))
  x <- dependency_status(m$targets[["data.csv"]], m$store, TRUE)
  expect_that(x[names(cmp)], equals(cmp))
  expect_that(sort(setdiff(names(x), names(cmp))),
              equals(sort(c("hash", "time"))))

  cmp <- list(version=m$store$version,
              name="processed",
              type="object",
              depends=list("data.csv"=NA_character_),
              fixed=NULL,
              code=list(
                functions=list(process_data=hash_function(
                                 m$store$env$env$process_data)),
                packages=list(utils=as.character(
                                packageVersion("utils")))))
  x <- dependency_status(m$targets[["processed"]], m$store, TRUE)
  expect_that(x[names(cmp)], equals(cmp))
  expect_that(sort(setdiff(names(x), names(cmp))),
              equals(sort(c("hash", "time"))))

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
              type="file",
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
  expect_that(res[names(cmp)], equals(cmp))
  expect_that(sort(setdiff(names(res), names(cmp))),
              equals(sort(c("hash", "time"))))

  ## Run the build system manually:
  remake_update(m, "data.csv")
  remake_update(m, "processed")
  remake_update(m, "plot.pdf")

  expect_that(remake_is_current(m, "data.csv"),  is_true())
  expect_that(remake_is_current(m, "processed"), is_true())
  expect_that(remake_is_current(m, "plot.pdf"),  is_true())

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

  expect_that(remake_update(m, "data.csv"), not(shows_message()))
  remake_update(m, "processed")
  remake_update(m, "plot.pdf")
  expect_that(file.exists("plot.pdf"), is_true())
  remake_make(m, "clean")
  expect_that(file.exists("plot.pdf"), is_false())
  expect_that(file.exists("data.csv"), is_true())

  cleanup()
})

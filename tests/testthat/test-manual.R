context("Manual run")

test_that("simple run", {
  cleanup()
  m <- remake("remake.yml")

  expect_false(remake_is_current(m, "data.csv"))
  expect_false(remake_is_current(m, "processed"))
  expect_false(remake_is_current(m, "plot.pdf"))

  ## This is possibly overkill:
  cmp <- list(version=m$store$version,
              name="data.csv",
              type="file",
              depends=empty_named_list(),
              fixed=hash_object(list("data.csv")),
              code=list(
                functions=list(download_data=hash_function(
                                 m$store$env$env$download_data))))
  x <- dependency_status(m$targets[["data.csv"]], m$store, TRUE)
  expect_equal(x[names(cmp)], cmp)
  expect_equal(sort(setdiff(names(x), names(cmp))),
               sort(c("hash", "time")))

  cmp <- list(version=m$store$version,
              name="processed",
              type="object",
              depends=list("data.csv"=NA_character_),
              fixed=NULL,
              code=list(
                functions=list(process_data=hash_function(
                                 m$store$env$env$process_data))))
  x <- dependency_status(m$targets[["processed"]], m$store, TRUE)
  expect_equal(x[names(cmp)], cmp)
  expect_equal(sort(setdiff(names(x), names(cmp))),
               sort(c("hash", "time")))

  ## NOTE: I don't really understand why we're sensitive to order
  ## here, differently from the command line and within R.  It seems
  ## to be how grDevices/graphics sort on lower/uppercase -- setting
  ## different locales perhaps?  This is dealt with by the
  ## identical_map() function within the package
  res <- dependency_status(m$targets[["plot.pdf"]], m$store, TRUE)
  pkgs <- c("grDevices", "graphics")
  cmp <- list(version=m$store$version,
              name="plot.pdf",
              type="file",
              depends=list(processed=NA_character_),
              fixed=hash_object(list("plot.pdf")),
              code=list(
                functions=list(
                  do_plot=hash_function(m$store$env$env$do_plot),
                  myplot=hash_function(m$store$env$env$myplot))))
  expect_equal(res[names(cmp)], cmp)
  expect_equal(sort(setdiff(names(res), names(cmp))),
               sort(c("hash", "time")))

  ## Run the build system manually:
  remake_update(m, "data.csv")
  remake_update(m, "processed")
  remake_update(m, "plot.pdf")

  expect_true(remake_is_current(m, "data.csv"))
  expect_true(remake_is_current(m, "processed"))
  expect_true(remake_is_current(m, "plot.pdf"))

  cleanup()
})

test_that("Depending on a file we don't make", {
  cleanup()
  ## Manually run the download step from before -- now we have a file
  ## that remake wants to depend on, but does not generate:
  e <- new.env()
  source("code.R", e)
  e$download_data("data.csv")
  expect_true(file.exists("data.csv"))

  ## This configuration is the same as remake.yml, but it does not
  ## contain a rule for building data.csv
  m <- remake("remake2.yml")

  expect_message(remake_update(m, "data.csv"), NA)
  remake_update(m, "processed")
  remake_update(m, "plot.pdf")
  expect_true(file.exists("plot.pdf"))
  remake_make(m, "clean")
  expect_false(file.exists("plot.pdf"))
  expect_true(file.exists("data.csv"))

  cleanup()
})

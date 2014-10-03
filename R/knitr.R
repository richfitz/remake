knitr_from_maker <- function(input, output, store, export,
                             export_source=TRUE, ...) {
  e <- new.env(parent=if (export_source) store$env$env else .GlobalEnv)
  store$objects$export(export, e)
  knitr::knit(input, output, envir=e, ...)
}

knitr_infer_source <- function(name) {
  if (!grepl("\\.md$", name)) {
    stop("Target must end in .md (at least for now)")
  }
  sub("\\.md$", "\\.Rmd", name)
}

knitr_default_fig_path <- function(filename) {
  sprintf("figure/%s__", tools::file_path_sans_ext(basename(filename)))
}

knitr_from_maker <- function(input, output, store, export,
                             export_source=TRUE,
                             knitr_options=NULL, chdir=FALSE,
                             auto_figure_prefix=FALSE, ...) {
  e <- new.env(parent=if (export_source) store$env$env else .GlobalEnv)
  store$objects$export(export, e)

  if (isTRUE(auto_figure_prefix) && is.null(knitr_options$fig.path)) {
    knitr_options$fig.path <- knitr_default_fig_path(output)
  }

  if (!is.null(knitr_options)) {
    ## Save the previous options:
    prev <- knitr::opts_chunk$get(names(knitr_options), drop=FALSE)
    on.exit(knitr::opts_chunk$set(prev))
    knitr::opts_chunk$set(knitr_options)
  }
  if (chdir) {
    owd <- setwd(dirname(input))
    on.exit(setwd(owd), add=TRUE)
    input <- basename(input)
    output <- basename(output)
  }
  knitr::knit(input, output, envir=e, ...)
}

knitr_infer_source <- function(name) {
  if (!grepl("\\.md$", name) & !grepl("\\.tex$", name) ) {
     stop("Target must end in .md or .tex (at least for now)")
   }
  name <- sub("\\.md$", "\\.Rmd", name)
  sub("\\.tex$", "\\.Rnw", name)
}

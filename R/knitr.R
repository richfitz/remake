knitr_default_fig_path <- function(filename) {
  sprintf("figure/%s__", tools::file_path_sans_ext(basename(filename)))
}

knitr_from_maker <- function(input, output, store, export,
                             export_source=TRUE,
                             knitr_options=NULL, ...) {
  e <- new.env(parent=if (export_source) store$env$env else .GlobalEnv)
  store$objects$export(export, e)
  if (!is.null(knitr_options)) {
    ## Save the previous options:
    prev <- knitr::opts_chunk$get(names(knitr_options), drop=FALSE)
    on.exit(knitr::opts_chunk$set(prev))
    knitr::opts_chunk$set(knitr_options)
  }
  knitr::knit(input, output, envir=e, ...)
}

knitr_infer_source <- function(name) {
  if (!grepl("\\.md$", name)) {
    stop("Target must end in .md (at least for now)")
  }
  sub("\\.md$", "\\.Rmd", name)
}

## This is all subject to change.  The idea is that we move upstream
## and find all object targets that might exist.
##
## It might be good to have a "strict" mode in which descent is not
## done, or is done to 1 level for fake targets and no levels for
## object targets.
##
## This is a bit convoluted because we go:
##   list -> names -> list -> names
## This is something that could be done more nicely I'm sure.
knitr_depends <- function(maker, depends) {
  graph <- maker$dependency_graph()
  depends_names <- dependencies(sapply(depends, function(x) x$name), graph)
  depends <- filter_targets_by_type(maker$get_targets(depends_names),
                                    "object")
  vapply(unname(depends), function(x) x$name, character(1))
}

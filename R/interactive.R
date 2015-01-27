## This file holds code for "interactive mode".  This is going to be
## useful for building makerfiles interactively.

interactive_parse <- function(expr) {
  e <- substitute(expr)
  ## Strip all braces:
  e <- interactive_drop_braces(expr)

}

interactive_drop_braces <- function(expr) {
  while (length(expr) > 1L && identical(expr[[1]], quote(`{`))) {
    if (length(expr) != 2L) {
      stop("Expected non-compound expression")
    }
    expr <- expr[[2]]
  }
  expr
}

interactive_check_assignment <- function(expr) {
  if (length(expr) == 0L ||
      (expr[[1]] != quote(`<-`) && expr[[1]] != quote(`=`))) {
    stop("Expected assignment operation")
  }
  name <- expr[[2]]
  if (!is.symbol(name)) {
    stop("Invalid target of assignment")
  }
  list(name=name, value=expr[[3]])
}

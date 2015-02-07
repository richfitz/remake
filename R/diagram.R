##' Plot the graph that remake generates.
##'
##' This is really just a placeholder, but I want this here early as
##' an indication of where the package is headed.  Plus this is
##' something I have always wanted in make.  Current version is not
##' tunable on purpose.
##' @title Make a figure with the dependency graph
##' @param remake_file Name of remake file (default is
##\code{remake.yml}).
##' @param ... Additional arguments thta control formatting but aren't
##' documented and are subject to change.
##' @export
diagram <- function(remake_file="remake.yml", ...) {
  m <- remake2(remake_file, load_sources=FALSE)
  str <- remake_diagram_command(m, ...)
  DiagrammeR::grViz(str)
}

diagram_nodes <- function(styles, classes) {
  assert_named_list(styles)
  assert_named_character(classes)
  fmt <- function(x) {
    sprintf("[ %s ]",
            paste(names(x), x, sep=" = ", collapse=", "))
  }
  node <- function(style, nodes) {
    sprintf("node %s %s",
            fmt(style),
            paste(squote(nodes), collapse="; "))
  }

  classes_uniq <- unique(classes)
  ## Also get the unstyled things *up* the list, perhaps?
  styles[setdiff(classes_uniq, names(styles))] <- list()

  nodes <- split(names(classes), classes)
  ret <- c(node(styles$base, character(0)),
           sapply(classes_uniq, function(x) node(styles[[x]], nodes[[x]]),
                  USE.NAMES=FALSE))
  paste(ret, collapse="\n")
}

diagram_edges <- function(mat) {
  nms <- rownames(mat)
  i <- which(mat, TRUE)
  paste(sprintf("'%s' -> '%s';", nms[i[,2]], nms[i[,1]]),
        collapse="\n")
}

## TODO: knitr/plot style too
diagram_styles_default <- function() {
  list(base=c(fontname="Helvetica"),
       fake=c(shape="box", color="blue"),
       file=c(shape="box", color="red"),
       object=c(shape="ellipse", color="green4"))
}

remake_diagram_command <- function(m, styles=NULL) {
  if (is.null(styles)) {
    styles <- diagram_styles_default()
  }

  g <- remake_private(m)$dependency_graph()

  ## Filter to exclude cleanup targets:
  types <- dependency_types(m$targets)
  keep <- types != "cleanup"
  g <- g[keep[names(g)]]
  types <- types[keep]
  mat <- dependencies_to_adjacency(g)

  nodes <- diagram_nodes(styles, types)
  edges <- diagram_edges(mat)

  sprintf("digraph remake { %s\n%s }", nodes, edges)
}

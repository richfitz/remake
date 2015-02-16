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

remake_diagram_command <- function(obj, styles=NULL) {
  if (is.null(styles)) {
    styles <- diagram_styles_default()
  }

  g <- remake_dependency_graph(obj)

  ## Filter to exclude cleanup targets:
  types <- vcapply(obj$targets, "[[", "type")
  keep <- types != "cleanup"
  g <- g[keep[names(g)]]
  types <- types[keep]
  mat <- dependencies_to_adjacency(g)

  nodes <- diagram_nodes(styles, types)
  edges <- diagram_edges(mat)

  sprintf("digraph remake { %s\n%s }", nodes, edges)
}

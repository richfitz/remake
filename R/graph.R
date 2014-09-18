## This algorithm comes from here:
## http://blog.jupo.org/2012/04/06/topological-sorting-acyclic-directed-graphs/
## and assumes that the graph is expressed as a *named* list.  The
## daughters of an element are its dependencies.
topological_order <- function(graph) {
  graph_sorted <- c()
  while (length(graph) > 0L) {
    acyclic <- FALSE
    for (node in names(graph)) {
      edges <- graph[[node]]
      if (!any(edges %in% names(graph))) {
        acyclic <- TRUE
        graph <- graph[names(graph) != node]
        graph_sorted <- c(graph_sorted, node)
        break
      }
    }

    if (!acyclic) {
      stop("A cyclic dependency detected")
    }
  }
  graph_sorted
}

topological_sort <- function(graph) {
  graph[topological_order(graph)]
}

dependencies <- function(node, graph) {
  seen <- structure(logical(length(graph)), names=names(graph))
  while (length(node) > 0L) {
    seen[node] <- TRUE
    kids <- unlist(unname(graph[node]))
    node <- kids[!seen[kids]]
  }
  names(seen[seen])
}

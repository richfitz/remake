##' Plot the graph that maker generates.
##'
##' This is really just a placeholder, but I want this here early as
##' an indication of where the package is headed.  Plus this is
##' something I have always wanted in make.  Current version is not
##' tunable on purpose.
##'
##' This function \emph{will} change.
##' @title Make a figure with the dependency graph
##' @param m A maker object
##' @param targets Optional character vector of targets.  If
##' specified, then only targets that these target depends on are
##' included.  Useful for filtering the graph.
##' @export
diagram <- function(m, targets=NULL) {
  g <- m$dependency_graph()
  if (!is.null(targets)) {
    g <- g[dependencies(targets, g)]
  }

  ## This wants abstracting.  Also worth thinking about getting rules
  ## onto the edges:
  dependencies_to_adjacency <- function(g) {
    t <- names(g)
    mat <- matrix(FALSE, length(t), length(t), dimnames=list(t, t))
    for (i in t) {
      ## TODO: empty dependency lists are list() not character(0)
      d <- g[[i]]
      if (length(d) > 0) {
        mat[i,d] <- TRUE
      }
    }
    mat
  }

  ## The transpose here will switch between the formal DAG thing, and
  ## the intuitive *flow*.
  op <- par(mar=rep(0, 4))
  on.exit(par(op))

  ## TODO: Deal with single node graph (layout fails)
  ## TODO: Colour nodes based on type
  library("igraph")
  mat <- dependencies_to_adjacency(g)
  gg <- graph.adjacency(t(mat))
  plot(gg,
       vertex.label.color="black", vertex.label.family="sans",
       vertex.shape="none", # easier
       edge.color="steelblue4",
       layout=layout.sugiyama(gg)$layout)
}

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

dependencies_to_adjacency <- function(graph) {
  t <- names(graph)
  mat <- matrix(FALSE, length(t), length(t), dimnames=list(t, t))
  for (i in t) {
    ## TODO: empty dependency lists are list() not character(0)
    d <- graph[[i]]
    if (length(d) > 0) {
      mat[i,d] <- TRUE
    }
  }
  mat
}

dependencies <- function(node, graph, dependencies_only=FALSE) {
  seen <- structure(logical(length(graph)), names=names(graph))
  nodes <- node
  while (length(nodes) > 0L) {
    seen[nodes] <- TRUE
    kids <- unlist(unname(graph[nodes]))
    nodes <- kids[!seen[kids]]
  }
  if (dependencies_only) {
    seen[node] <- FALSE
  }
  names(seen[seen])
}


## The situation that will be hardest is if a target will be rebuild
## later in the tree.  For that reason, I think that we need to do a
## couple of different passes here to make sure that everything will
## behave sensibly.
##
## The issue I have is if A depends on B but is OK with state of B,
## we'd still want to rebuild A if B changes because of *any other*
## upstream change.
##
## So, the correct way to do this seems:
##
## * Work out the players involved in a graph
## * Work out which are not current: they are *dirty*
## * Start at the beginning of the graph and work out if a target
##   needs rebuilding.  If a target needs rebuilding, then flag
##   everything that depends on it as *potentially dirty*.
## * Special care needed for things that are check: exists/check: code
##   though (i.e., not check_depends()).  There is no point moving
##   past the dependency there.
status <- function(target_name, graph, m) {
  candidates <- dependencies(target_name, graph)

  ## Now, work up the tree working out if these are dirty or dirty by
  ## descent (dbd):
  dirty <- dbd <- structure(logical(length(candidates)), names=candidates)
  for (t in candidates) {
    x <- m$get_target(t)
    dirty[[t]] <- !x$is_current()
    if (check_depends(x$check)) {
      d <- sapply(x$dependencies_real(), function(x) x$name)
      ## TODO: This conditional goes away if we have a dependency
      ## names function that always returns a character vector.
      if (length(d) > 0L) {
        dbd[[t]] <- any(dirty[d]) || any(dbd[d])
      }
    }
  }

  cbind(dirty=dirty, dirty_by_descent=dbd)
}

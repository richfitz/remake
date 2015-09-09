diagram_nodes <- function(styles, base) {
  fmt <- function(x) {
    sprintf("[ %s ]", paste(names(x), x, sep=" = ", collapse=", "))
  }
  styles_fmt <- unname(apply(styles, 1, fmt))
  styles_i <- match(styles_fmt, unique(styles_fmt))
  styles_uniq <- styles_fmt[!duplicated(styles_i)]
  nodes <- unname(tapply(squote(rownames(styles)), styles_i, paste,
                         collapse="; "))
  dat <- sprintf("node %s %s",
                 c(fmt(base), styles_uniq),
                 c("", nodes))
  paste(dat, collapse="\n")
}

diagram_edges <- function(mat, include_tooltips=TRUE) {
  nms <- rownames(mat)
  i <- which(!is.na(mat), TRUE)
  if (include_tooltips) {
    str <- sprintf("'%s' -> '%s' [tooltip = '%s'];",
                   nms[i[,2]], nms[i[,1]], mat[i])
  } else {
    str <- sprintf("'%s' -> '%s';",
                   nms[i[,2]], nms[i[,1]])
  }
  paste(str, collapse="\n")
}

remake_diagram_command <- function(obj) {
  g <- remake_dependency_graph(obj)

  ## Filter to exclude cleanup targets:
  ## Do this with list_targets:
  keep <- remake_list_targets(obj, type=NULL,
                              include_implicit_files=TRUE,
                              include_cleanup_targets=FALSE,
                              include_chain_intermediates=TRUE)

  types <- vcapply(obj$targets[keep], "[[", "type")
  g <- g[keep]
  mat <- dependencies_to_adjacency(g, obj)

  ## Sort out the file subtypes:
  i <- types == "file"
  file_subtype <- vcapply(obj$targets[names(which(i))],
                          function(x) class(x)[[1]])
  types[i] <- sub("^target_", "", file_subtype)

  ## Styles; all of this will move into some function that accepts
  ## "palette" as an option and then we can use some tricks to fill
  ## out the names sensibly.
  colours <- c(fake="#34495e",     # wet asphalt
               file="#d35400",     # pumkin
               file_implicit="#1abc9c", # turqoise
               knitr="#c0392b",    # pomegranate
               plot="#f1c40f",     # orange
               download="#f1c40f", # sunflower
               object="#3498db")   # peter river
  shape <- c(fake="circle",
             file="box",
             file_implicit="box",
             knitr="box",
             plot="box",
             download="box",
             object="ellipse")
  current <- remake_is_current(obj, names(g))
  font <- "courier"
  fontsize <- 10
  fill_fraction <- 0.2

  fill <- mix_cols(colours, "white", fill_fraction)
  names(fill) <- names(colours)

  ## Then, from this make the styles:
  t <- types[names(g)]
  styles <- cbind(shape=shape[t],
                  color=squote(colours[t]),
                  fillcolor=squote(ifelse(current, colours[t], fill[t])),
                  style="filled")
  rownames(styles) <- names(g)

  nodes <- diagram_nodes(styles, c(fontname=font, fontsize=fontsize))

  edges <- diagram_edges(mat)

  sprintf("digraph remake { %s\n%s }", nodes, edges)
}

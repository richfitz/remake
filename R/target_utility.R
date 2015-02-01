utility_install_packages <- function(m) {
  extras <- unique(unlist(lapply(m$targets, function(x) x$packages)))
  install_packages(union(m$store$env$packages, extras))
}

utility_gitignore <- function(m) {
  files <- filter_targets_by_type(m$targets, "file")
  add_to_gitignore(c(".maker", files))
}

install_packages <- function(packages,
                             maker_sources="maker_sources.yml") {
  pkgs <- rownames(installed.packages())
  msg <- setdiff(packages, pkgs)

  if (length(msg) > 0L) {
    message("Installing missing required packages:\n",
            paste0("\t", msg, collapse="\n"))
    if (file.exists(maker_sources)) {
      extras <- read_maker_packages(maker_sources)
      extras <- extras[names(extras) %in% msg]
    } else {
      extras <- list()
    }

    from_cran <- setdiff(msg, names(extras))
    if (length(from_cran) > 0L) {
      install.packages(msg)
    }
    if (length(extras) > 0L) {
      install_extras(extras)
    }
  }
}

## This could be made way more clever, but that's just asking for
## trouble.  We could install into a gitignore that is elsewhere up
## the tree, we could deal with m not using the current directory as
## the path.  This is the simplest case though.
##
## Getting this working for hgignore is much harder though as that
## does have to go down to the root directory.  Getting that working
## will require a path-diff function, and that's going to play badly
## with things like symbolic links, etc, etc.
add_to_gitignore <- function(files) {
  if (file.exists(".gitignore")) {
    curr <- readLines(".gitignore")
    files <- setdiff(files, strip_whitespace(curr))
    if (length(files) > 0) {
      writeLines(c(curr, files), ".gitignore")
    }
  } else {
    writeLines(files, ".gitignore")
  }
}

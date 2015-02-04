utility_install_packages <- function(m) {
  extras <- unique(unlist(lapply(m$targets, function(x) x$packages)))
  install_packages(union(m$store$env$packages, extras))
}

utility_gitignore <- function(m) {
  files <- filter_targets_by_type(m$targets, "file")
  add_to_gitignore(c(".maker", files))
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

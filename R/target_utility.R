utility_deps <- function(m) {
  install_dependencies(m$packages)
  m$load_sources()
}

utility_gitignore <- function(m) {
  files <- names(m$get_targets_by_type("file"))
  add_to_gitignore(c(".maker", files))
}

## This is super basic for now: don't try and do anything clever with
## non-CRAN packages or the like.  Rather than do that we should
## probably try and leverage the packrat automatic dependency
## detection and its lockfile.
install_dependencies <- function(packages, ...) {
  pkgs <- rownames(installed.packages())
  msg <- setdiff(packages, pkgs)
  if (length(msg) > 0L) {
    message("Installing missing required packages:\n",
            paste0("\t", msg, collapse="\n"))
    install.packages(msg, ...)
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

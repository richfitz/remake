create_environment <- function(sources, packages) {
  e <- new.env(parent=.GlobalEnv)
  for (p in packages) {
    library(p, character.only=TRUE)
  }
  for (s in sources) {
    if (is_directory(s)) {
      source_dir(s, e, chdir=TRUE)
    } else {
      source(s, e, chdir=TRUE)
    }
  }
  e
}

source_dir <- function(path, ..., pattern="^.*\\.\\[Rr]$") {
  files <- dir(path, pattern, full.names=TRUE)
  for (f in files) {
    source(f, ...)
  }
}

create_environment <- function(sources, packages) {
  e <- new.env(parent=.GlobalEnv)
  for (p in packages) {
    library(p, character.only=TRUE)
  }
  for (s in sources) {
    if (is_directory(s)) {
      for (f in dir(s, pattern="^.*\\.[Rr]$", full.names=TRUE)) {
        source(f, e, chdir=TRUE)
      }
    } else {
      source(s, e, chdir=TRUE)
    }
  }
  e
}

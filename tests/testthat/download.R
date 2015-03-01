read_data <- function(src, dest) {
  e <- new.env(parent=baseenv())
  sys.source(src, e)
  write.csv(e$mtcars, dest)
}

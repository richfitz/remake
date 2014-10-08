## Functions for dealing with maker archives.

## Test if this really is a make archive.
## 1. Has a single top level directory
## 2. Contains second level directories "db", "objects", "files"
is_maker_archive <- function(filename) {
  assert_file_exists(filename)

  contents <- unzip(filename, list=TRUE)
  if (nrow(contents) == 0L) {
    return(FALSE)
  }
  paths <- path_split(contents$Name)
  tld <- unique(sapply(paths, function(x) x[[1]]))
  if (length(tld) > 1L) {
    return(FALSE)
  }

  ## Second level directories.  I see a trailing directory slash on my
  ## computer (OSX) but am not sure if that is portable.  Also note
  ## that extra directories are fine here.
  sld <- paste0(file.path(tld, c("db", "objects", "files")), "/")
  if (!all(sld %in% contents$Name)) {
    return(FALSE)
  }

  TRUE
}

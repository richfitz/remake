## Functions for dealing with maker archives.

## Test if this really is a make archive.
## 1. Has a single top level directory
## 2. Contains second level directories "db", "objects", "files"
is_maker_archive <- function(filename, error=FALSE) {
  assert_file_exists(filename)

  contents <- unzip(filename, list=TRUE)
  if (nrow(contents) == 0L) {
    if (error) {
      stop("Not a maker archive: zipfile is empty")
    }
    return(FALSE)
  }
  paths <- path_split(contents$Name)
  tld <- maker_archive_tld(filename, error=error)
  if (length(tld) > 1L) {
    return(FALSE)
  }

  ## Second level directories.  I see a trailing directory slash on my
  ## computer (OSX) but am not sure if that is portable.  Also note
  ## that extra directories are fine here.
  sld <- paste0(file.path(tld, c("db", "objects", "files")), "/")
  if (!all(sld %in% contents$Name)) {
    if (error) {
      stop("Not a maker archive: expected directories db, objects, files")
    }
    return(FALSE)
  }

  TRUE
}

assert_maker_archive <- function(filename) {
  is_maker_archive(filename, error=TRUE)
}

maker_archive_tld <- function(filename, error=TRUE) {
  contents <- unzip(filename, list=TRUE)
  paths <- path_split(contents$Name)
  tld <- unique(sapply(paths, function(x) x[[1]]))
  if (length(tld) > 1L && error) {
    stop("Not a maker archive: expected single top level directory")
  }
  tld
}

maker_archive_contents <- function(filename) {
  assert_maker_archive(filename)
  tld <- maker_archive_tld(filename, error=TRUE)
  contents <- unzip(filename, list=TRUE)
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive=TRUE))
  re <- paste0("^", file.path(tld, "db"), ".*\\.rds")
  keep <- contents$Name[grepl(re, contents$Name)]
  res <- unzip(filename, exdir=path, files=keep)
  unname(sapply(res, function(x) readRDS(x)$name))
}

maker_archive_import <- function(maker, filename) {
  contents <- maker_archive_contents(filename)
  if (!all(maker$has_target(contents))) {
    stop("Objects in archive not known to maker: ",
         paste(setdiff(contents, maker$target_names()), collapse=", "))
  }

  tld <- maker_archive_tld(filename)

  path <- tempfile()
  dir.create(path)
  unzip(filename, exdir=path)
  path <- file.path(path, tld)

  for (t in contents) {
    maker$get_target(t)$archive_import(path)
  }
}

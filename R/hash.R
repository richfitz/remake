## Hashing wrappers.  First, lock in the current behaviour:
hash_object <- function(value) {
  digest::digest(value)
}

hash_function <- function(f) {
  ## Work around a hard-to-trigger bug: this global option affects
  ## deparsing!
  oo <- options(scipen=0)
  on.exit(options(oo))
  digest::digest(deparse(f))
}

hash_files <- function(filenames, named=TRUE) {
  if (is.null(filenames)) {
    filenames <- character(0)
  }
  hash <- tools::md5sum(filenames)
  if (named) hash else unname(hash)
}

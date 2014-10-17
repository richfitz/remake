## Hashing wrappers.  First, lock in the current behaviour:
hash_object <- function(value) {
  digest(value)
}

hash_files <- function(filenames, named=TRUE) {
  hash <- tools::md5sum(filenames)
  if (named) hash else unname(hash)
}

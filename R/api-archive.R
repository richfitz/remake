##' Export an archive of remake contents.  Implicit files (those that
##' remake does not generate) are not exported.
##'
##' @section Warning:
##' The archive format is subject to change and is
##' not suitable for long-term archiving.  Moreover, it depends on R's
##' internal rds format.  This format is itself not guaranteed to stay
##' constant, though it has for a long time now (see
##' [serialize()]).  However, this is likely to be
##' reasonable for data interchange between computers or for
##' short/medium term export of results.  Until a lossless
##' representation of all R objects exists, the rds problem is not
##' likely to go away.
##' @title Export remake contents
##' @param target_names Names of targets to export.
##' @param dependencies Export the \emph{dependencies} of
##' `target_names`?  The default is `TRUE`, which allows
##' targets such as `all` to be specified in order to export
##' everything that is a dependency of `all`.  If
##' `dependencies` is `FALSE`, all elements of
##' `target_names` must represent files or objects.
##' @param verbose Be verbose when reading the remake file?
##' @param require_current Logical indicating if the targets must be
##' up-to-date to be fetched.  If this is `TRUE` and the targets
##' are not up-to-date, then an error will be thrown.
##' @param allow_missing_packages Allow missing packages when loading
##' remake file?
##' @param archive_file Name of the archive file to generate, by
##' default `remake.zip`.
##' @param remake_file Remake file to read, by default
##' `remake.yml`.
##' @return Invisibly, the name of the archive file generated.
##' However, this function is primarily useful for its side effect,
##' which is generating the archive.
##' @export
archive_export <- function(target_names=NULL, dependencies=TRUE,
                           verbose=TRUE, require_current=TRUE,
                           allow_missing_packages=FALSE,
                           archive_file="remake.zip",
                           remake_file="remake.yml") {
  obj <- remake(remake_file, verbose=verbose,
                load_sources=require_current,
                allow_missing_packages=allow_missing_packages)
  remake_archive_export(obj, target_names,
                        dependencies=dependencies,
                        require_current=require_current,
                        archive_file=archive_file)
}

##' Import a previously exported archive (see
##' [archive_export()].  This function will overwrite files
##' and objects.  Be careful.
##' @title Import a remake archive
##' @param archive_file Name of the zip file to import from
##' @param verbose Be verbose when reading the remake file?
##' @param allow_missing_packages Allow missing packages when loading
##' remake file?
##' @param remake_file Remake file to read, by default
##' `remake.yml`.
##' @export
archive_import <- function(archive_file="remake.zip",
                           verbose=TRUE, allow_missing_packages=FALSE,
                           remake_file="remake.yml") {
  obj <- remake(remake_file, verbose=verbose,
                allow_missing_packages=allow_missing_packages)
  remake_archive_import(obj, archive_file)
}

## Test if this really is a make archive.
## 1. Has a single top level directory
## 2. Contains second level directories "db", "objects", "files"
## TODO: Save remake version information so that old versions can be
## handled.
##
## NOTE: reserve the right to change the format.

##' Test if a file is a remake archive.
##' @title Test if a zip file is likely to be a remake archive, as
##' created by `archive_create`
##' @param archive_file Name of a file, by default `remake.zip`
##' (the default for [archive_export()].
##' @export
is_archive <- function(archive_file="remake.zip") {
  assert_file_exists(archive_file)

  contents <- unzip(archive_file, list=TRUE)
  if (nrow(contents) == 0L) { # empty
    return(FALSE)
  }
  tld <- remake_archive_tld(archive_file, error=FALSE)
  if (length(tld) > 1L) { # more than one top level direcyory
    return(FALSE)
  }

  ## Require the metadata:
  file.path(tld, "remake.rds") %in% contents$Name
}

##' List contents of a remake archive
##'
##' NOTE: At present this unzips the entire archive.
##' @title List contents of a remake archive
##' @param archive_file Name of the zip file to read from, by default
##' "remake.zip".
##' @param detail Return a data frame with more detail?
##' @return A character vector with the contents of the archive.
##' @export
list_archive <- function(archive_file="remake.zip", detail=FALSE) {
  ## TODO: implement long format:
  ##   name
  ##   type
  ##   hash
  ##   date
  assert_remake_archive(archive_file)
  tld <- remake_archive_tld(archive_file, error=TRUE)
  contents <- unzip(archive_file, list=TRUE)
  path <- tempfile()
  dir.create(path, recursive=TRUE)
  on.exit(file_remove(path, recursive=TRUE))

  unzip(archive_file, exdir=path)

  ## TODO: factor this bit out:
  st <- storr::storr_rds(file.path(path, tld, "objects"),
                         default_namespace="remake_db",
                         mangle_key=TRUE)
  keys <- st$list()

  db <- lapply(keys, function(x) st$get(x))
  db_names <- vcapply(db, function(x) x$name, USE.NAMES=FALSE)

  if (detail) {
    db_type <- vcapply(db, function(x) x$type, USE.NAMES=FALSE)
    ## TODO: This is far from ideal, but I don't see how to get times
    ## into a data.frame easily.
    db_time <- rep(Sys.time(), length(db_names))
    for (i in seq_along(db_time)) {
      db_time[[i]] <- db[[i]]$time
    }
    db_hash  <- vcapply(db, function(x) x$hash, USE.NAMES=FALSE)
    ret <- data.frame(type=db_type,
                      time=db_time,
                      hash=db_hash,
                      stringsAsFactors=FALSE)
    rownames(ret) <- db_names
  } else {
    ret <- db_names
  }
  ret
}

##' Fetch a file or object from an archive.  Throws an error if
##' requesting a target that was not exported (see
##' [list_archive()] for archive contents).
##' @title Fetch a file or object from an archive
##' @param target_name Name of a single file or
##' @param path_prefix Optional path prefix for exported files only
##' (not for objects).  If given, a file `path/to/file` will be
##' exported as `path_prefix/path/to/file`; this is useful to avoid
##' overwriting existing data, as this will happen without warning.
##' @param archive_file Name of the archive file to use, by
##' default `remake.zip`.
##' @return If `target_name` refers to an object, then the return
##' value is the restored object.  If `target_name` refers to a
##' file, then the return value is the path to the restored file
##' (including `path_prefix` if given).
##' @export
##' @author Rich FitzJohn
fetch_archive <- function(target_name,
                          path_prefix=NULL,
                          archive_file="remake.zip") {
  assert_scalar_character(target_name)
  assert_remake_archive(archive_file)

  path <- tempfile()
  dir.create(path, recursive=TRUE)
  on.exit(file_remove(path, recursive=TRUE))

  unzip(archive_file, exdir=path)
  tld <- remake_archive_tld(archive_file)

  st_db <- storr::storr_rds(file.path(path, tld, "objects"),
                            default_namespace="remake_db",
                            mangle_key=TRUE)
  if (!st_db$exists(target_name)) {
    stop(target_name, " not found in archive ", archive_file)
  }

  type <- st_db$get(target_name)$type

  if (type == "file") {
    v <- archive_get_file(file.path("files", target_name),
                          path, archive_file)
    if (is.null(path_prefix)) {
      ret <- target_name
    } else {
      ret <- file.path(path_prefix, target_name)
    }
    dir.create(dirname(ret), FALSE, TRUE)
    file.copy(v, ret, overwrite=TRUE)
  } else if (type == "object") {
    st <- storr::storr_rds(file.path(path, tld, "objects"))
    ret <- st$get(target_name)
  } else {
    stop("Can't extract target of type ", type)
  }
  ret
}

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
  dir.create(path, recursive=TRUE)
  on.exit(unlink(path, recursive=TRUE))
  re <- paste0("^", file.path(tld, "db"), ".*\\.rds")
  keep <- contents$Name[grepl(re, contents$Name)]
  res <- unzip(filename, exdir=path, files=keep)
  unname(sapply(res, function(x) readRDS(x)$name))
}

archive_export_maker <- function(maker, target_name, recursive=TRUE,
                                 filename="maker.zip") {
  if (recursive) {
    graph <- maker$dependency_graph()
    target_name <- dependencies(target_name, graph)
  }
  targets <- maker$get_targets(target_name)
  path <- file.path(tempfile(),
                    tools::file_path_sans_ext(basename(filename)))
  store <- maker$store
  dir.create(path, recursive=TRUE)
  for (t in filter_targets_by_type(targets, c("file", "object"))) {
    archive_export_target(t, store, path, missing_ok=FALSE)
  }
  zip_dir(path)
}

archive_import_maker <- function(maker, filename) {
  contents <- maker_archive_contents(filename)
  if (!all(maker$has_target(contents))) {
    stop("Objects in archive not known to maker: ",
         paste(setdiff(contents, maker$target_names()), collapse=", "))
  }

  tld <- maker_archive_tld(filename)

  path <- tempfile()
  dir.create(path, recursive=TRUE)
  unzip(filename, exdir=path)
  path <- file.path(path, tld)
  store <- maker$store

  for (t in contents) {
    archive_import_target(maker$get_target(t), store, path)
  }
}

archive_export_target <- function(target, store, path, missing_ok=FALSE) {
  check_archivable(target$type)
  assert_directory(path)

  if (target$type == "file") {
    path_files <- file.path(path, "files")
    store$files$archive_export(target$name, path_files, missing_ok)
  } else if (target$type == "object") {
    path_objects <- file.path(path, "objects")
    store$objects$archive_export(target$name, path_objects, missing_ok)
  }

  path_db <- file.path(path, "db")
  store$db$archive_export(target$name, path_db, missing_ok)
}

archive_import_target <- function(target, store, path) {
  check_archivable(target$type)
  assert_directory(path)

  if (target$type == "file") {
    store$files$archive_import(target$name, file.path(path, "files"))
  } else {
    store$objects$archive_import(target$name, file.path(path, "objects"))
  }

  path_db <- file.path(path, "db")
  store$db$archive_import(target$name, path_db)
}

check_archivable <- function(type) {
  if (!(type == "file" || type == "object")) {
    stop("Not something that can be copied")
  }
}

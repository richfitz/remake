remake_archive_export <- function(obj, target_names=NULL,
                                  dependencies=TRUE,
                                  require_current=TRUE,
                                  archive_file="remake.zip") {
  target_names <- remake_default_target(obj, target_names)
  if (dependencies) {
    ## Make sure that we *don't* include fake targets, implicit files,
    ## etc.
    ## NOTE: These options match those in remake_archive_import
    target_names <-
      remake_list_dependencies(obj, target_names,
                               type=c("file", "object"),
                               include_implicit_files=FALSE,
                               include_cleanup_targets=FALSE)
  } else {
    pos <-
        remake_list_targets(obj,
                            type=c("file", "object"),
                            include_implicit_files=FALSE,
                            include_cleanup_targets=FALSE)
    if (!all(target_names %in% pos)) {
      stop("Non-archivable target specified: ",
           paste(setdiff(target_names, pos), collapse=", "))
    }
  }
  if (require_current) {
    assert_is_current(obj, target_names)
  }

  remake_print_message(obj, "ZIP", archive_file,
                       paste(target_names, collapse=", "),
                       "curly")
  path <- file.path(tempfile(),
                    tools::file_path_sans_ext(basename(archive_file)))
  dir.create(path, recursive=TRUE)
  on.exit(file_remove(path, recursive=TRUE))

  store <- obj$store
  for (t in obj$targets[target_names]) {
    archive_export_target(t, store, path)
  }
  saveRDS(archive_metadata(), file.path(path, "remake.rds"))

  zip_dir(path, archive_file)
}

remake_archive_import <- function(obj, archive_file) {
  contents <- list_archive(archive_file)

  ## NOTE: These options match those in remake_archive_export
  known_targets <-
    remake_list_targets(obj,
                        type=c("file", "object"),
                        include_implicit_files=FALSE,
                        include_cleanup_targets=FALSE)

  if (!all(contents %in% known_targets)) {
    stop("Objects in archive not known to remake: ",
         paste(setdiff(contents, known_targets), collapse=", "))
  }

  remake_print_message(obj, "UNZIP", archive_file,
                       paste(contents, collapse=", "),
                       "curly")

  tld <- remake_archive_tld(archive_file)

  path <- tempfile()
  dir.create(path, recursive=TRUE)
  on.exit(file_remove(path, TRUE))
  unzip(archive_file, exdir=path)
  path <- file.path(path, tld)
  store <- obj$store

  for (t in obj$targets[contents]) {
    archive_import_target(t, store, path)
  }
}

## Utilities for working with the archive:
remake_archive_tld <- function(archive_file, error=TRUE) {
  contents <- unzip(archive_file, list=TRUE)
  paths <- path_split(contents$Name)
  tld <- unique(vcapply(paths, function(x) x[[1]]))
  if (length(tld) > 1L && error) {
    stop("Not a remake archive: expected single top level directory")
  }
  tld
}

## TODO: What about knitr associated files?  These are going to be
## really hard to get.  Ideally we'd grab the results of automatically
## munging the figure directory I think.  Could be worth a warning.
## Ideally we'd use something in knitr to tell us what figures were
## generated; I think I have notes about this somewhere.  Probably
## what should happen is for knitr targets we should try to work up a
## manifest of what was generated while running the target.  Might pay
## to ask about this, as it seems generally useful.
archive_export_target <- function(target, store, path) {
  if (target$type == "file") {
    store$files$archive_export(target$name, file.path(path, "files"))
  } else if (target$type == "object") {
    if (store$objects$exists(target$name)) {
      store$objects$archive_export(file.path(path, "objects"), target$name)
    } else {
      stop(sprintf("key %s not found in object store", target$name))
    }
  }
  store$db$archive_export(file.path(path, "objects"), target$name)
}

archive_import_target <- function(target, store, path) {
  if (target$type == "file") {
    store$files$archive_import(target$name, file.path(path, "files"))
  } else {
    store$objects$archive_import(file.path(path, "objects"),
                                 target$name)
  }
  store$db$archive_import(file.path(path, "objects"), target$name)
}

## TODO: Copy in some general remake metadata, including:
##   1. session info
##   2. remake version
##   3. user hook?
## The question is -- what would we *do* with that information?  The
## remake version bits would be helpful if we upgrade remake and
## need to move things around in the store.  The session info is
## useful for stashing information about what was run for printing.
## The user hook is most dubiously useful, but could be useful in
## practice.
archive_metadata <- function() {
  ## TODO: Get local directory git information in here, including
  ## incompletely committed files.  See richfitz/tree for an example.
  list(remake_version=packageVersion("remake"),
       session_info=devtools::session_info())
}

archive_get_file <- function(path, dest_dir, archive_file) {
  tld <- remake_archive_tld(archive_file)
  unzip(archive_file, file.path(tld, path), exdir=dest_dir)
}

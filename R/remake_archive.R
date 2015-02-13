remake_archive_export <- function(obj, target_names, dependencies=TRUE,
                                  archive_file="remake.zip") {
  ## TODO: Need to check that things are current first.
  ## TODO: Don't include implicit targets
  ## TODO: Use filter targets to make sure that implicit things are
  ## not included, but *do* include chain targets, but don't include
  ## cleanup!
  if (dependencies) {
    ## Make sure that we *do* include chain intermediates, but make
    ## sure that we *don't* include fake targets, implicit files,
    ## etc.
    ## NOTE: These options match those in remake_archive_import
    target_names <-
      remake_list_dependencies(obj, target_names,
                               type=c("file", "object"),
                               include_implicit_files=FALSE,
                               include_cleanup_targets=FALSE,
                               include_chain_intermediates=TRUE)
  } else {
    pos <-
        remake_list_targets(obj,
                            type=c("file", "object"),
                            include_implicit_files=FALSE,
                            include_cleanup_targets=FALSE,
                            include_chain_intermediates=TRUE)
    if (!all(target_names %in% pos)) {
      stop("Non-archivable target specified: ",
           paste(setdiff(target_names, pos), collapse=", "))
    }
  }

  assert_is_current(obj, target_names)

  path <- file.path(tempfile(),
                    tools::file_path_sans_ext(basename(archive_file)))
  dir.create(path, recursive=TRUE)
  on.exit(unlink(path, recursive=TRUE))

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
                        include_cleanup_targets=FALSE,
                        include_chain_intermediates=TRUE)

  if (!all(contents %in% known_targets)) {
    stop("Objects in archive not known to remake: ",
         paste(setdiff(contents, known_targets), collapse=", "))
  }

  tld <- remake_archive_tld(archive_file)

  path <- tempfile()
  dir.create(path, recursive=TRUE)
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
  missing_ok <- FALSE
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
  if (target$type == "file") {
    store$files$archive_import(target$name, file.path(path, "files"))
  } else {
    store$objects$archive_import(target$name, file.path(path, "objects"))
  }

  path_db <- file.path(path, "db")
  store$db$archive_import(target$name, path_db)
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

##' Install missing packages
##'
##' No version comparison is done - see packrat for a more complete
##' package management solution.
##' @title Install missing packages
##' @param maker_file Name of the makerfile to look for the list of
##' required packages
##' @param instructions Rather than install anything, prints
##' instructions on how to install missing things
##' @param missing_only Install \emph{everything}, rather than just
##' missing packages.
##' @param skip_target_packages Skip packages that are mentioned only
##' in targets?
##' @export
##' @author Rich FitzJohn
install_missing_packages <- function(maker_file="maker.yml",
                                     instructions=FALSE,
                                     missing_only=TRUE,
                                     skip_target_packages=FALSE) {
  dat <- read_maker_file(maker_file)
  packages <- with_default(dat$packages, character(0))
  if (!skip_target_packages) {
    packages <- c(packages,
                  unlist(lapply(dat$targets, function(x) x$packages)))
  }
  package_sources <- read_maker_packages("maker_sources.yml")
  ret <- install_packages(packages,
                          instructions=instructions,
                          missing_only=missing_only,
                          package_sources=package_sources)
  if (instructions) {
    message(ret)
  }
  invisible(packages)
}

install_packages <- function(packages,
                             instructions=FALSE,
                             missing_only=TRUE,
                             package_sources=NULL) {
  if (missing_only) {
    packages <- missing_packages(packages)
  }

  if (length(packages) > 0L) {
    if (!instructions) {
      message("Installing missing required packages:\n",
              paste0("\t", packages, collapse="\n"))
    }

    if (!is.null(package_sources)) {
      extras <- package_sources[names(package_sources) %in% packages]
      from_cran <- setdiff(packages, names(extras))
      if (length(extras) > 0L) {
        if (missing_only) {
          from_cran <- c(from_cran, missing_packages("devtools"))
        } else {
          from_cran <- c(from_cran, "devtools")
        }
      }
    } else {
      extras <- list()
      from_cran <- packages
    }

    str_cran  <- install_packages_cran(from_cran, instructions)
    str_extra <- install_packages_extra(extras, instructions)

    c(str_cran, str_extra)
  } else {
    character(0)
  }
}

install_packages_cran <- function(packages, instructions=FALSE) {
  if (length(packages) == 0L) {
    return(character(0))
  }
  if (instructions) {
    sprintf("install.packages(%s)",
            paste(dquote(packages), collapse=", "))
  } else {
    install.packages(packages)
    packages
  }
}

## We assume that the packages listed in package_sources don't have
## complicated dependencies and all come *after* the packages on
## CRAN.  So the CRAN packages are installed and *then* the packages
## here are installed.  It's not wonderful, but it will work.
##
## Packages that depend on github packages will fail if we go through
## sequentially.  But if we go through simultaneously we can't easily
## pass in extra arguments.  There's no way of winning here without a
## more comprehensive set of package infrastructure.  This is where
## packrat shines, but that's just too much here.
install_packages_extra <- function(dat, instructions=FALSE) {
  if (length(dat) == 0L) {
    return(character(0))
  }
  common <- names(formals(devtools::install))
  install_packages_extra1 <- function(package) {
    x <- dat[[package]]
    fn_name <- install_function(x$source)
    fn_r <- getExportedValue("devtools", fn_name)
    pos <- union(names(formals(fn_r)), common)
    opts <- intersect(setdiff(names(x), "source"), pos)
    if (instructions) {
      x_opts <- x[opts]
      i <- vlapply(x_opts, is.character)
      x_opts[i] <- lapply(x_opts[i], dquote)
      j <- match(pos[[1]], names(x_opts))
      if (!is.na(j)) {
        x_opts <- c(unname(x_opts[j]), x_opts[-j])
      }
      do_call_fake(paste0("devtools::", fn_name), x_opts)
    } else {
      do.call(fn_r, x[opts])
      package
    }
  }
  sapply(names(dat), install_packages_extra1, USE.NAMES=FALSE)
}

missing_packages <- function(packages) {
  setdiff(packages, .packages(TRUE))
}

## Not trying to support much of what packrat does: just trying to
## keep it reasonably simple.
read_maker_packages <- function(filename) {
  required <- list(github="repo",
                   bitbucket="repo",
                   url="url",
                   git="git_url")
  if (file.exists(filename)) {
    dat <- yaml_read(filename)
  } else {
    dat <- NULL
  }
  for (i in names(dat)) {
    x <- dat[[i]]
    x$name <- i
    src <- match_value(x$source, names(required))
    msg <- setdiff(required[[src]], names(x))
    if (length(msg) > 0L) {
      stop(sprintf("Required fields missing from %s: %s",
                   i, paste(msg, collapse=", ")))
    }
    dat[[i]] <- x
  }
  dat
}

install_function <- function(src) {
  switch(src,
         github="install_github",
         bitbucket="install_bitbucket",
         url="install_url",
         git="install_git",
         stop("Invalid source ", src))
}

missing_packages_condition <- function(packages) {
  msg <- paste("Some packages are missing:",
               paste(packages, collapse=", "))
  cond <- list(message=msg, packages=packages)
  class(cond) <- c("missing_packages", "condition")
  stop(cond)
}

missing_packages_recover <- function(e, m) {
  extra <- read_maker_packages("maker_sources.yml")
  file <- maker_private(m)$file
  packages <- e$packages
  if (getOption("maker.install.missing.packages", FALSE)) {
    install_packages(packages,
                     instructions=FALSE,
                     package_sources=extra)
    m$store$env$reload(TRUE)
  } else {
    indent <- function(x) paste0("    ", x)
    str_manual <- indent(install_packages(packages,
                                          instructions=TRUE,
                                          package_sources=extra))
    if (is.null(file)) {
      str_maker <- character(0)
    } else {
      str_maker <- sprintf('maker::install_missing_packages("%s")',
                           file)
      str_maker <- c(indent(str_maker), "or:")
    }

    str <- paste(c(e$message, "Install with:", str_maker, str_manual),
                 collapse="\n")
    stop(str, call.=FALSE)
  }
}

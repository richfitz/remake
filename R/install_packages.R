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

    ## Check args:
    fn_r <- getExportedValue("devtools", fn_name)
    pos <- union(names(formals(fn_r)), common)
    opts <- intersect(setdiff(names(x), "source"), pos)

    ## Move first argument to the first position and unname it, to
    ## match canonical style:
    x_opts <- x[opts]
    i <- match(pos[[1]], names(x_opts))
    if (!is.na(i)) {
      x_opts <- c(unname(x_opts[i]), x_opts[-i])
    }

    call <- as.call(c(list(as.symbol(fn_name)), x_opts))

    if (instructions) {
      ## devtools::install_<source> usually fully qualified:
      paste0("devtools::", deparse(call))
    } else {
      ## A bit of faff involved here:
      e <- new.env(parent=.GlobalEnv)
      assign(fn_name, fn_r, e)
      eval(call, e)
      package
    }
  }

  vcapply(names(dat), install_packages_extra1, USE.NAMES=FALSE)
}

missing_packages <- function(packages) {
  setdiff(packages, .packages(TRUE))
}

## Not trying to support much of what packrat does: just trying to
## keep it reasonably simple.
read_remake_packages <- function(filename) {
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

missing_packages_recover <- function(e, obj) {
  extra <- read_remake_packages("remake_sources.yml")
  file <- obj$file
  packages <- e$packages
  if (getOption("remake.install.missing.packages", FALSE)) {
    install_packages(packages,
                     instructions=FALSE,
                     package_sources=extra)
    obj$store$env$reload(TRUE)
  } else {
    indent <- function(x) paste0("    ", x)
    str_manual <- indent(install_packages(packages,
                                          instructions=TRUE,
                                          package_sources=extra))
    if (is.null(file)) {
      str_remake <- character(0)
    } else {
      str_remake <- sprintf('remake::install_missing_packages("%s")',
                           file)
      str_remake <- c(indent(str_remake), "or:")
    }

    str <- paste(c(e$message, "Install with:", str_remake, str_manual),
                 collapse="\n")
    stop(str, call.=FALSE)
  }
}

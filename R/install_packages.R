## Eventually this is going to support a lot of what packrat does, but
## for now trying to keep it reasonably simple.
read_maker_packages <- function(filename) {
  required <- list(github="repo",
                   bitbucket="repo",
                   url="url",
                   git="git_url")
  dat <- yaml_read(filename)
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
install_extras <- function(dat) {
  common <- names(formals(devtools::install))
  for (x in dat) {
    f <- install_function(x$source)
    pos <- union(names(formals(f)), common)
    opts <- intersect(names(x), setdiff(pos, "source"))
    do.call(f, x[opts])
  }
}

install_function <- function(src) {
  switch(src,
         github=devtools::install_github,
         bitbucket=devtools::install_bitbucket,
         url=devtools::install_url,
         git=devtools::install_git,
         stop("Invalid source ", src))
}

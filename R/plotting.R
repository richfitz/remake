## TODO: Some of this is probably done better and more tested in
## knitr.  See if we can leverage that (I don't immediately see
## anything obvious).

## Pretty much just a placeholder for now; only switches two
## types. for now.
##
## TODO: Some of these might want to set different arguments based on
## the extension.  So eps and ps might want different options.
get_device <- function(name) {
  name <- tolower(name)
  switch(name,
         pdf="pdf",
         png="png",
         stop("Unsupported device ", name))
}

## Open a device with a bunch of arguments.  Then check that this did
## actually open.  This is because any function `dev` can be passed
## through and there is no way of determining that this is actually a
## plotting device.
open_device <- function(dev, args, envir=.GlobalEnv) {
  prev <- dev.cur()
  do.call(dev, args, envir=envir)
  if (identical(dev.cur(), prev)) {
    stop("Failed to open a plotting device")
  }
}

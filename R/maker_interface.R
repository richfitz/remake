## Functions for interfacing with maker objects.  Not all of these
## will be supported going forward.

## TODO: I'm not sure this one is actually useful.
make_dependencies <- function(m, target_name, ...) {
  private <- maker_private(m)
  t <- private$get_target(target_name)
  ## TODO: I don't see the big problem here:
  if (!(t$type) %in% c("object", "file")) {
    warning(sprintf("%s is not a real target"))
  }

  m$refresh()
  maker_private(m)$print_message("ENV", t$name, style="angle")
  private$make1(target_name, ..., dependencies_only=TRUE)
  deps_name <- t$depends_name[t$depends_type == "object"]

  invisible(maker_environment(m, deps_name, t))
}

maker_script <- function(m, target_name=NULL) {
  private <- maker_private(m)
  if (is.null(target_name)) {
    target_name <- private$target_default()
  }
  pkgs <- lapply(m$store$env$packages,
                 function(x) sprintf('library("%s")', x))
  ## TODO: This does not work for *directories*.  Emit the body of
  ## source_dir with appropriate things set.
  srcs <- lapply(m$store$env$sources,
                 function(x) sprintf('source("%s")', x))
  ## Probably best to filter by "real" here?
  plan <- private$plan(target_name)
  cmds <- lapply(plan, function(i)
    target_run_fake(m$targets[[i]], for_script=TRUE))

  src <- c(unlist(pkgs),
           unlist(srcs),
           unlist(cmds))
  class(src) <- "maker_script"
  src
}

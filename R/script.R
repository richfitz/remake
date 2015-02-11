remake_script <- function(m, target_name=NULL) {
  target_name <- remake_default_target(m, target_name)
  pkgs <- lapply(m$store$env$packages,
                 function(x) sprintf('library("%s")', x))
  srcs <- lapply(m$store$env$find_files(),
                 function(x) sprintf('source("%s")', x))
  ## Probably best to filter by "real" here?
  plan <- remake_plan(m, target_name)
  cmds <- lapply(plan, function(i)
    target_run_fake(m$targets[[i]], for_script=TRUE))

  ## Need to make missing paths.
  files <- filter_targets_by_type(m$targets[plan], "file")
  paths <- dirname(names(files))
  ## Implicit targets exist...
  implicit <- vlapply(files, inherits, "target_file_implicit")
  ## ...so these paths must already exist...
  paths_existing <- unique(c(".", paths[implicit]))
  ## ...and these need creating:
  paths_to_create <- setdiff(unique(paths[!implicit]), paths_existing)
  cmds <- c(sprintf('dir.create("%s", FALSE, TRUE)', paths_to_create),
            cmds)
  
  c(unlist(pkgs),
    unlist(srcs),
    unlist(cmds))
}

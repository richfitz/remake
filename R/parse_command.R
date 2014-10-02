## There will be *two* possible way of getting
## target_argument_name in:
##  - use the target name, possibly with quotes (test that!)
##  - use the special name target_name, *no quotes*.  This then
##    becomes a restricted name in target_reserved_names.
## (currently quoting or not is not checked)
parse_target_command <- function(target, command) {
  dat <- parse_command(command)
  if (length(dat$depends) > 0L) {
    targets <- sapply(dat$depends, "[[", 1)
    pos <- c(target, "target_name")
    ## Need to determine that there is only a single possible target:
    i <- sapply(pos, function(x) targets == x)
    if (length(targets) == 1) {
      i <- rbind(i, deparse.level=0)
    }

    if (sum(i) == 1L) {
      j <- which(rowSums(i) == 1L)
      if (is.null(names(dat$depends)) || names(dat$depends)[[j]] == "") {
        dat$target_argument <- j
      } else {
        dat$target_argument <- names(dat$depends)[[j]]
      }
      dat$depends <- dat$depends[-j]
    } else if (sum(i) > 1L) {
      n <- colSums(i)
      n <- n[n > 0]
      stop(sprintf("target name matched multiple times in command for '%s': %s",
                   dat$rule,
                   paste(sprintf("%s (%d)", names(n), n), collapse=", ")))
    }
  }
  dat
}

parse_command <- function(str) {
  res <- check_command(str)
  rule <- check_command_rule(res[[1]])
  depends <- lapply(res[-1], check_command_arg)
  list(rule=rule, depends=depends)
}

check_command <- function(str) {
  res <- parse(text=as.character(str))
  if (length(res) != 1L) {
    stop("Expected single expression")
  }
  res <- res[[1]]
  if (length(res) == 0) {
    stop("I don't think this is possible")
  }
  if (!is.call(res)) {
    stop("Expected a function call (even with no arguments)")
  }
  res
}

check_command_arg <- function(x) {
  if (is.name(x)) {
    x <- as.character(x)
  } else if (!is.character(x)) {
    stop("Every element must be a character or name")
  }
  x
}

check_command_rule <- function(x) {
  if (is.name(x)) {
    x <- as.character(x)
  } else if (!is.character(x)) {
    stop("Rule must be a character or name")
  }
  x
}

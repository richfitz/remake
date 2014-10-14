## There will be *two* possible way of getting
## target_argument in:
##  - use the target name, *in quotes*
##  - use the special name target_name, *no quotes*.  This then
##    becomes a restricted name in target_reserved_names.
parse_target_command <- function(target, command) {
  dat <- parse_command(command)
  if (length(dat$depends) > 0L) {
    ## This whole section tries to work out the target_argument field.
    targets <- sapply(dat$depends, "[[", 1)
    pos <- c(target, "target_name")
    ## Need to determine that there is only a single possible target:
    i <- sapply(pos, function(x) targets == x)
    if (length(targets) == 1) {
      i <- rbind(i, deparse.level=0)
    }

    if (sum(i) == 1L) {
      j <- unname(which(rowSums(i) == 1L))
      if (is.null(names(dat$depends)) || names(dat$depends)[[j]] == "") {
        dat$target_argument <- j
      } else {
        dat$target_argument <- names(dat$depends)[[j]]
      }
      if (dat$depends[[j]] == "target_name" && dat$quoted[[j]]) {
        stop("target_name must not be quoted (it's like a variable)")
      }
      if (dat$depends[[j]] != "target_name" && !dat$quoted[[j]]) {
        stop("target name must be quoted (it must be a file name)")
      }
      dat$depends <- dat$depends[-j]
      dat$quoted  <- dat$quoted[-j] # TODO: allows target name through
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

parse_target_chain <- function(target, chain) {
  chain <- lapply(chain, parse_target_command, target=target)

  has_dot <- sapply(chain, function(x) "." %in% x$depends)

  if ("." %in% has_dot[[1]]) {
    stop("The first element in a chain cannot contain a dot ('.')")
  }
  if (any(!has_dot[-1])) {
    stop("All chain elements except the first need a dot")
  }
  has_target_argument <- sapply(chain, function(x) !is.null(x$target_argument))
  nok <- has_target_argument & seq_along(chain) < length(chain)
  if (any(nok)) {
    stop("Can only refer to target in the final element of a chain")
  }
  chain
}

parse_command <- function(str) {
  res <- check_command(str)
  rule <- check_command_rule(res[[1]])
  quoted <- logical(length(res) - 1)
  ## This could be done more tidily!
  depends <- lapply(res[-1], check_command_arg)
  quoted <- vapply(depends, function(x) attr(x, "quoted"), logical(1),
                   USE.NAMES=FALSE)
  depends <- lapply(depends, as.character)
  list(rule=rule, depends=depends, quoted=quoted)
}

check_command <- function(str) {
  assert_scalar_character(str)
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
    attr(x, "quoted") <- FALSE
  } else if (is.character(x)) {
    attr(x, "quoted") <- TRUE
  } else {
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

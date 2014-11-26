## Will change name soon, but the basic idea is to sort out what it is
## that we have to run:
##
## TODO: Need some tests here, throughout
process_target_command <- function(name, dat) {
  core <- c("command", "depends",
            "rule", "target_argument", "quoted", "depends_is_arg",
            "chain")

  ## Quick check that may disappear later:
  invalid <- c("rule", "target_argument", "quoted")
  if (any(invalid %in% names(dat))) {
    stop("Invalid keys: ",
         paste(intersect(invalid, names(dat)), collapse=", "))
  }

  if (length(dat$depends) > 0) {
    ## TODO: this might come through as a proper yaml map list
    ##   depends:
    ##    - data: processed
    ## or improperly as
    ##   depends:
    ##     data: processed
    ## The contortions below do a reasonable job of dealing with this,
    ## but it's not enough.
    dat$depends <- unlist(from_yaml_map_list(dat$depends))
  } else {
    dat$depends <- character(0)
  }

  dat$depends_is_arg <- rep(FALSE, length(dat$depends))

  if (!is.null(dat$command)) {
    cmd <- parse_target_command(name, dat$command)

    if ("depends" %in% names(dat)) { # extra dependencies:
      if (is.null(cmd$chain)) {
        cmd$depends <- c(cmd$depends, dat$depends)
        cmd$depends_is_arg <- c(cmd$depends_is_arg,
                                dat$depends_is_arg)
      } else {
        cmd$chain[[1]]$depends <- c(cmd$chain[[1]]$depends, dat$depends)
        cmd$chain[[1]]$depends_is_arg <- c(cmd$chain[[1]]$depends_is_arg,
                                           dat$depends_is_arg)
      }
    }

    dat[intersect(names(cmd), core)] <- cmd
  }

  type <- target_infer_type(name, dat)

  is_command <- names(dat) %in% core
  list(command=dat[is_command], opts=dat[!is_command], type=type)
}

## There will be *two* possible way of getting
## target_argument in:
##  - use the target name, *in quotes*
##  - use the special name target_name, *no quotes*.  This then
##    becomes a restricted name in target_reserved_names.
parse_target_command <- function(target, command) {
  if (length(command) > 1L) {
    ## This is an early exit, which is slightly evil but avoids this
    ## whole function being a big if/else statement.
    return(parse_target_chain(target, command))
  }

  dat <- parse_command(command)
  if (length(dat$depends) > 0L) {
    ## This whole section tries to work out the target_argument field.
    targets <- dat$depends

    ## Deal with dots first.
    if (any(targets == "." & dat$quoted)) {
      stop("Dot argument must not be quoted (it's like a variable)")
    }
    if (sum(targets == ".") > 1L) {
      stop("Only a single dot argument allowed")
    }

    ## Then with target_name
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
  dat$depends_is_arg <- rep_along(TRUE, dat$depends)
  dat
}

parse_target_chain <- function(target, chain) {
  chain <- lapply(chain, parse_target_command, target=target)

  ## TODO: Check >1 dot
  has_dot <- sapply(chain, function(x) "." %in% x$depends)

  if (has_dot[[1]]) {
    stop("The first element in a chain cannot contain a dot ('.')")
  }
  if (any(!has_dot[-1])) {
    stop("All chain elements except the first need a dot")
  }

  len <- length(chain)
  has_target_argument <- sapply(chain, function(x) !is.null(x$target_argument))
  if (any(has_target_argument & seq_len(len) < len)) {
    stop("Can only refer to target in the final element of a chain")
  }

  ret <- chain[[len]]
  ret$chain <- chain[-len]
  ret
}

parse_command <- function(str) {
  res <- check_command(str)
  rule <- check_command_rule(res[[1]])
  quoted <- logical(length(res) - 1)
  ## This could be done more tidily!
  depends <- lapply(res[-1], check_command_arg)
  quoted <- vapply(depends, function(x) attr(x, "quoted"), logical(1),
                   USE.NAMES=FALSE)
  depends <- vapply(depends, as.character, character(1))
  list(rule=rule, depends=depends, quoted=quoted)
}

check_command <- function(str) {
  assert_scalar_character(str)
  res <- parse(text=as.character(str), keep.source=FALSE)
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

target_infer_type <- function(name, dat) {
  type <- dat$type
  if (is.null(type)) {
    type <- if (target_is_file(name)) "file" else  "object"
    if ("knitr" %in% names(dat)) {
      type <- "knitr"
    } else if ("plot" %in% names(dat)) {
      type <- "plot"
    } else if (type == "object" && is.null(dat$command)) {
      type <- "fake"
    }
  } else {
    assert_scalar_character(type)
  }
  type
}

##' Determine if a target is treated as a file or not.
##'
##' A target is a file if it contains a slash anywhere in the name, or
##' if it ends in one of the known extensions.  The current set of
##' known file extensions is available as \code{\link{extensions}()},
##' but soon will become configurable.
##' @title Determine if target is a file
##' @param x Vector of target names
##' @return A logical vector, the same length as \code{x}
##' @export
target_is_file <- function(x) {
  is_file <- grepl("/", x, fixed=TRUE)
  check <- !is_file
  if (any(check)) {
    is_file[check] <- tolower(file_extension(x[check])) %in% extensions()
  }
  is_file
}

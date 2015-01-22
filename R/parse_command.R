## Will change name soon, but the basic idea is to sort out what it is
## that we have to run:
##
## TODO: Need some tests here, throughout
process_target_command <- function(name, dat) {
  core <- c("command", "rule", "args", "depends", "is_target",
            "depends_rename", "chain")

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
    deps <- unlist(from_yaml_map_list(dat$depends))
    dat$depends <- structure(rep(NA_integer_, length(deps)), names=deps)
    dat$depends_rename <- deps
  } else {
    dat$depends <- empty_named_integer()
    dat$depends_rename <- empty_named_character()
  }

  if (!is.null(dat$command)) {
    cmd <- parse_target_command(name, dat$command)

    if ("depends" %in% names(dat) && length(dat$depends > 0)) {
      if (is.null(cmd$chain)) {
        cmd$depends <- c(cmd$depends, dat$depends)
      } else {
        cmd$chain[[1]]$depends <- c(cmd$chain[[1]]$depends, dat$depends)
      }
    }

    rewrite <- intersect(names(cmd), core)
    dat[rewrite] <- cmd[rewrite]
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
    targets <- names(dat$depends)

    ## Deal with dots first (move into parse_command?)
    is_dot <- targets == "."
    if (sum(is_dot) > 1L) {
      stop("Only a single dot argument allowed")
    } else if (sum(is_dot) == 1L) {
      i <- which(is_dot)
      if (is.character(dat$args[[i]])) {
        stop("Dot argument must not be quoted (it's like a variable)")
      }
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

      ## Checking (purely to keep things consistent -- this would
      ## actually be fine)
      v <- dat$args[[j]]
      if (is.character(v) && v == "target_name") {
        stop("target_name must not be quoted (it's like a variable)")
      } else if (is.name(v) && v != quote(target_name)) {
        stop("target name must be quoted (it must be a file name)")
      }

      ## Then remove target_name from the dependencies.
      dat$args[[j]] <- target
      dat$depends <- dat$depends[-j]
      dat$is_target[[j]] <- FALSE
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

  ## TODO: Check >1 dot
  ## TODO: Drop "%in%"
  has_dot <- vlapply(chain, function(x) "." %in% names(x$depends))

  if (has_dot[[1]]) {
    stop("The first element in a chain cannot contain a dot ('.')")
  }
  if (any(!has_dot[-1])) {
    stop("All chain elements except the first need a dot")
  }

  calls_target <- function(x) {
    any(vlapply(x$args, identical, target))
  }
  len <- length(chain)
  has_target_argument <- vlapply(chain, calls_target)
  if (any(has_target_argument & seq_len(len) < len)) {
    stop("Can only refer to target in the final element of a chain")
  }

  ret <- chain[[len]]
  ret$chain <- chain[-len]
  ret
}

## I think this is where I need to intervene -- rebuild this from the
## ground up.
##
## I think that numbers and logical values could autmatically be
## passed through as as-is once we're done here, so make sure not to
## jepordise that.
parse_command <- function(str) {
  res <- check_command(str)
  rule <- check_command_rule(res[[1]])

  ## OK, here's the template onto which we'll inject arguments.
  args <- as.list(res[-1])

  ## First, test for target-like-ness.  That will be things that are
  ## names or character only.  Numbers, etc will drop through here and
  ## we'll pick them up shortly.
  is_target <- unname(vlapply(args, is_target_like))

  if (any(!is_target)) {
    args[!is_target] <- lapply(args[!is_target], check_literal_arg)
  }

  depends <- structure(which(is_target),
                       names=vcapply(args[is_target], as.character,
                         USE.NAMES=FALSE))
  
  list(rule=rule, args=args, depends=depends, is_target=is_target)
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

check_command_rule <- function(x) {
  if (is.name(x)) {
    x <- as.character(x)
  } else if (!is.character(x)) {
    stop("Rule must be a character or name")
  }
  x
}

## The trick here is going to be working out which of these need later
## looking up, if we allow this.
check_literal_arg <- function(x) {
  if (is.atomic(x)) { # logical, integer, complex types
    x
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(I))) {
      x[[2]]
    } else {
      ## This error message is not going to be useful:
      stop("Unknown special function ", as.character(x[[1]]))
    }
  } else {
    stop("Unknown type in argument list")
  }
}

is_target_like <- function(x) {
  is.character(x) || is.name(x)
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

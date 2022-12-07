#' @export
#' @family display
format.effectclass <- function(x, ..., type = c("ascii", "markdown")) {
  type <- match.arg(type)
  is_effectclass(x, message = "error")
  levels(x) <- switch(
    type,
    markdown = sprintf("`%s`", levels(x)),
    levels(x)
  )
  iconv(as.character(x), from = "UTF8", to = "UTF8")
}

#' @export
#' @importFrom assertthat has_name
print.effectclass <- function(x, ...) {
  dots <- list(...)
  if (has_name(dots, "sep")) {
    sep <- dots$sep
  } else {
    sep <- " "
  }
  cat(format(x, ...), sep = sep)
}

#' @export
`[.effectclass` <- function(x, ...) {
  is_effectclass(x, message = "error")
  structure(
    factor(as.character(x)[...], levels = levels(x)),
    signed = attr(x, "signed"),
    detailed = attr(x, "detailed"),
    class = c("effectclass", "factor")
  )
}

#' Flatten Lists
#' @export
#' @inheritParams base::unlist
#' @seealso base::unlist
#' @family utils
unlist <- function(x, recursive = TRUE, use.names = TRUE) { # nolint
  UseMethod("unlist")
}

#' @export
#' @inheritParams base::unlist
unlist.default <- function(x, recursive = TRUE, use.names = TRUE) { #nolint
  is_list <- vapply(x, is.list, TRUE)
  if (recursive && any(is_list)) {
    for (i in which(is_list)) {
      x[[i]] <- unlist(x[[i]], recursive = recursive, use.names = use.names)
    }
  }
  effect_ok <- vapply(x, is_effectclass, TRUE, message = "none")
  if (all(!effect_ok)) {
    return(base::unlist(x = x, recursive = recursive, use.names = use.names))
  }
  if (!all(effect_ok)) {
    stop("all elements or no elements should be `effectclass`")
  }
  unlist.effectclass(x = x, recursive = recursive, use.names = use.names)
}

#' @export
#' @inheritParams base::unlist
unlist.effectclass <- function(x, recursive = TRUE, use.names = TRUE) { #nolint
  if (!is.list(x)) {
    is_effectclass(x, message = "error")
    return(x)
  }
  vapply(x, is.list, TRUE) |>
    which() -> is_list
  for (i in is_list) {
    x[[i]] <- unlist(x[[i]], recursive = recursive, use.names = use.names)
  }
  vapply(x, is_effectclass, TRUE, message = "error")
  signed <- unique(vapply(x, attr, TRUE, which = "signed"))
  detailed <- unique(vapply(x, attr, TRUE, which = "detailed"))
  assert_that(
    length(signed) == 1,
    msg = "all elements should be either signed or unsigned"
  )
  assert_that(
    length(detailed) == 1,
    msg = "all elements should be either detailed or coarse"
  )
  vapply(
    x, FUN.VALUE = vector("list", 1L),
    FUN = function(z) {
      list(as.character(z))
    }
  ) |>
    do.call(what = c) |>
    factor(levels = levels(x[[1]])) |>
    structure(
      signed = signed, detailed = detailed, class = c("effectclass", "factor")
    )
}

#' @export
#' @inheritDotParams base::c
c.effectclass <- function(...) {
  dots <- list(...)
  unlist(dots)
}

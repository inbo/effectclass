#' @export
format.effectclass <- function(x, ..., type = c("ascii", "markdown", "arrow")) {
  type <- match.arg(type)
  is_effectclass(x, message = "error")
  if (type == "markdown") {
    levels(x) <- sprintf("`%s`", levels(x))
  }
  if (type == "arrow") {
    if (attr(x, "signed")) {
      levels(x) <- gsub("\\+", "\u2191", levels(x))
      levels(x) <- gsub("-", "\u2193", levels(x))
      levels(x) <- gsub("~", "\u2195", levels(x))
    } else {
      levels(x) <- gsub("\\*", "\u2192", levels(x))
      levels(x) <- gsub("~", "\u2194", levels(x))
    }
  }
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
  base::unlist(x = x, recursive = recursive, use.names = use.names)
}

#' @export
#' @inheritParams base::unlist
unlist.effectclass <- function(x, recursive = TRUE, use.names = TRUE) { #nolint
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
  structure(
    factor(vapply(x, as.character, NA_character_), levels = levels(x[[1]])),
    signed = signed,
    detailed = detailed,
    class = c("effectclass", "factor")
  )
}

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
  NextMethod()
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

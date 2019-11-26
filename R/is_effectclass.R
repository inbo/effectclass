#' Check If an Object Is a Valid Effectclass Object
#' @param x The object to test.
#' @param message What to do when the object is not a valid effectclass object.
#' `"none"`: return `FALSE` with a message.
#' `"warning"`: return `FALSE` with a `warning()`.
#' `"error"`: return an error.
#' @return A single `TRUE` or `FALSE` value.
#' @export
is_effectclass <- function(x, message = c("none", "warning", "error")) {
  UseMethod("is_effectclass", x)
}

#' @export
is_effectclass.default <- function(x, message = c("none", "warning", "error")) {
  message <- match.arg(message)
  msg <- "x is not an 'effectclass' object"
  switch(message, warning = warning(msg), error = stop(msg))
  return(FALSE)
}

check_attr <- function(x, attribute, message) {
  if (!has_attr(x, attribute)) {
    msg <- paste0("x is missing the '", attribute, "' attribute")
    switch(message, warning = warning(msg), error = stop(msg))
    return(TRUE)
  }
  if (!is.flag(attr(x, attribute)) || !noNA(attr(x, attribute))) {
    msg <- paste0("the '", attribute,
                  "' attribute must be a single TRUE or FALSE")
    switch(message, warning = warning(msg), error = stop(msg))
    return(TRUE)
  }
  return(FALSE)
}

#' @export
#' @importFrom assertthat assert_that is.flag noNA has_attr
is_effectclass.effectclass <- function(
  x, message = c("none", "warning", "error")
) {
  message <- match.arg(message)
  if (!inherits(x, "factor")) {
    msg <- "x is not a factor"
    switch(message, warning = warning(msg), error = stop(msg))
    return(FALSE)
  }
  problems <- check_attr(x = x, attribute = "signed", message = message) ||
    check_attr(x = x, attribute = "detailed", message = message)
  if (problems) {
    return(FALSE)
  }
  target_levels <- list(
    c("*", "~", "?"),
    c("**", "*", "*~", "~", "?*", "?"),
    c("+", "~", "-", "?"),
    c("++", "+", "+~", "~", "-~", "-", "--", "?+", "?-", "?")
  )[[attr(x, "signed") * 2 + attr(x, "detailed") + 1]]
  if (length(levels(x)) != length(target_levels)) {
    msg <- sprintf(
      "%s, %s effectclass object requires %i levels",
      ifelse(attr(x, "signed"), "a signed", "an unsigned"),
      ifelse(attr(x, "detailed"), "detailed", "coarse"),
      length(target_levels)
    )
    switch(message, warning = warning(msg), error = stop(msg))
    return(FALSE)
  }
  if (!identical(levels(x), target_levels)) {
    msg <- sprintf(
      "%s, %s effectclass object requires following levels:\n%s",
      ifelse(attr(x, "signed"), "a signed", "an unsigned"),
      ifelse(attr(x, "detailed"), "detailed", "coarse"),
      paste0("'", target_levels, "'", collapse = ", ")
    )
    switch(message, warning = warning(msg), error = stop(msg))
    return(FALSE)
  }
  return(TRUE)
}

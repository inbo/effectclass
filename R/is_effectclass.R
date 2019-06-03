#' Check if an object is a valid effectclass object
#' @param x the object to test
#' @param message what to do when the object is not a valid effectclass object. `"none"`: return `FALSE` with a message. `"warning"`: return `FALSE` with a `warning()`. `"error"`: return an error.
#' @return a single `TRUE` or `FALSE` value
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
  if (!has_attr(x, "signed")) {
    msg <- "x is missing the 'signed' attribute"
    switch(message, warning = warning(msg), error = stop(msg))
    return(FALSE)
  }
  if (!has_attr(x, "detailed")) {
    msg <- "x is missing the 'detailed' attribute"
    switch(message, warning = warning(msg), error = stop(msg))
    return(FALSE)
  }
  if (!is.flag(attr(x, "signed"))) {
    msg <- "the 'signed' attribute must be a single TRUE or FALSE"
    switch(message, warning = warning(msg), error = stop(msg))
    return(FALSE)
  }
  if (!noNA(attr(x, "signed"))) {
    msg <- "the 'signed' attribute must be a single TRUE or FALSE"
    switch(message, warning = warning(msg), error = stop(msg))
    return(FALSE)
  }
  if (!is.flag(attr(x, "detailed"))) {
    msg <- "the 'detailed' attribute must be a single TRUE or FALSE"
    switch(message, warning = warning(msg), error = stop(msg))
    return(FALSE)
  }
  if (!noNA(attr(x, "detailed"))) {
    msg <- "the 'detailed' attribute must be a single TRUE or FALSE"
    switch(message, warning = warning(msg), error = stop(msg))
    return(FALSE)
  }
  if (attr(x, "signed")) {
    if (attr(x, "detailed")) {
      if (length(levels(x)) != 10) {
        msg <- "a signed, detailed effectclass object requires 10 levels"
        switch(message, warning = warning(msg), error = stop(msg))
        return(FALSE)
      }
      if (!identical(
        levels(x),
        c("++", "+", "+~", "~", "-~", "-", "--", "?+", "?-", "?")
      )) {
        msg <- "a signed, detailed effectclass object requires following levels:
'++', '+', '+~', '~', '-~', '-', '--', '?+', '?-', '?'"
        switch(message, warning = warning(msg), error = stop(msg))
        return(FALSE)
      }
    } else {
      if (length(levels(x)) != 4) {
        msg <- "a signed, coarse effectclass object requires 4 levels"
        switch(message, warning = warning(msg), error = stop(msg))
        return(FALSE)
      }
      if (!identical(levels(x), c("+", "~", "-", "?"))) {
        msg <- "a signed, coarse effectclass object requires following levels:
'+', '~', '-', '?'"
        switch(message, warning = warning(msg), error = stop(msg))
        return(FALSE)
      }
    }
  } else {
    if (attr(x, "detailed")) {
      if (length(levels(x)) != 6) {
        msg <- "an unsigned, detailed effectclass object requires 6 levels"
        switch(message, warning = warning(msg), error = stop(msg))
        return(FALSE)
      }
      if (!identical(
        levels(x),
        c("**", "*", "*~", "~", "?*", "?")
      )) {
msg <- "an unsigned, detailed effectclass object requires following levels:
'**', '*', '*~', '~', '?*', '?'"
        switch(message, warning = warning(msg), error = stop(msg))
        return(FALSE)
      }
    } else {
      if (length(levels(x)) != 3) {
        msg <- "an unsigned, coarse effectclass object requires 3 levels"
        switch(message, warning = warning(msg), error = stop(msg))
        return(FALSE)
      }
      if (!identical(levels(x), c("*", "~", "?"))) {
msg <- "an unsigned, coarse effectclass object requires following levels:
'*', '~', '?'"
        switch(message, warning = warning(msg), error = stop(msg))
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

change_pretty <- function(log_x) {
  original <- exp(log_x) - 1
  scale <- floor(log10(abs(original)))
  scale[is.infinite(scale)] <- 1
  scaled <- round(abs(original / 10 ^ scale), 2)
  nice <- ifelse(scaled >= 2, ceiling(scaled), ceiling(scaled * 10) / 10)
  log(1 + sign(original) * nice * 10 ^ scale)
}

#' Logarithmic breaks for changes
#'
#' Breaks a set of pretty breaks for changes.
#' @param n the number of breaks on either side of the reference
#' @export
#' @importFrom assertthat assert_that is.count
#' @importFrom utils head tail
#' @family utils
change_breaks <- function(n = 2) {
  assert_that(is.count(n))
  n_default <- n
  function(x, n = n_default) {
    if (length(x) == 0) {
      return(numeric(0))
    }
    extreme <- max(abs(x))
    positive <- change_pretty(seq(0, extreme, length = n + 1))
    c(change_pretty(rev(-tail(positive, -1))), positive)
  }
}

#' Display logarithmic changes as percentage
#' @param x the logarithmic changes
#' @export
#' @family utils
change_labels <- function(x) {
  assert_that(is.numeric(x))
  percent <- 100 * exp(x) - 100
  if (!any(abs(percent) > 1e-8)) {
    return(sprintf(paste0("%+.0f%%"), percent))
  }
  smallest <- min(abs(percent[abs(percent) > 1e-8]), na.rm = TRUE)
  magnitude <- -floor(log10(smallest))
  magnitude <- magnitude + as.integer(smallest * 10 ^ magnitude < 2)
  sprintf(paste0("%+.", pmax(magnitude, 0), "f%%"), percent)
}

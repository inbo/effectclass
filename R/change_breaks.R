change_pretty <- function(log_x) {
  original <- exp(log_x) - 1
  scale <- floor(log10(abs(original)))
  scale[is.infinite(scale)] <- 1
  scaled <- round(abs(original / 10 ^ scale), 2)
  nice <- ifelse(scaled >= 2, ceiling(scaled), ceiling(scaled * 10) / 10)
  log(1 + sign(original) * nice * 10 ^ scale)
}

#' Logaritmic breaks for changes
#'
#' Breaks a set of pretty breaks for changes.
#' @param n the number of breaks on either side of the reference
#' @export
#' @importFrom assertthat assert_that is.count
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

#' Display logaritmic changes as percentage
#' @export
change_labels <- function(x) {
  percent <- 100 * exp(x) - 100
  if (all(percent == 0)) {
    return(sprintf(paste0("%+.0f%%"), percent))
  }
  smallest <- min(abs(percent[percent != 0]))
  magnitude <- abs(floor(log10(smallest)))
  magnitude <- magnitude + as.integer(smallest * 10 ^ magnitude < 2)
  sprintf(paste0("%+.", magnitude, "f%%"), percent)
}

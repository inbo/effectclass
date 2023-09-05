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
    abs(x) |>
      max() |>
      exp() -> extreme
    magnitude <- log10(extreme)
    (10 ^ seq(0, magnitude, by = 1)) |>
      outer(1:9) |>
      as.vector() |>
      c(
        3 / 2, 4 / 3, 5 / 3, 5 / 4, 10 / 9, 20 / 19, 25 / 24, 50 / 49, 100 / 99,
        200 / 199, 500 / 499, 1000 / 999, 2000 / 1999, 5000 / 4999, 1e4 / 9999
      ) |>
      sort() -> candidate
    candidate[candidate <= extreme] |>
      c(
        head(candidate[candidate > extreme], 1)
      ) -> candidate
    rel_position <- log(candidate) / max(log(candidate))
    seq(0, 1, length = n + 1) |>
      outer(rel_position, "-") -> delta
    selected <- candidate[apply(delta ^ 2, 1, which.min)]
    rev(1 / selected) |>
      head(-1) |>
      c(selected) |>
      log()
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

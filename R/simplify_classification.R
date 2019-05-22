#' Use a lower scale classification
#'
#' @description
#' `simplify_classification()` reduces the 10 scales from `classification()`
#' to the 4 scales below.
#' - `+` **positive effect**: reference < lcl
#' - `~` **no effect**: min(threshold) < lcl < reference and reference < ucl < max(threshold)
#' - `-` **negative effect**: ucl < reference
#' - `?` **unknown effect**: lcl < min(threshold) or max(threshold) < ucl
#'
#' `simplify_classification()` reduces the 6 scales from
#' `remove_sign(classification())` into 3 scales.
#' @inheritParams remove_sign
#' @export
#' @importFrom assertthat assert_that
simple_classification <- function(classification) {
  assert_that(is.factor(classification))
  simplified <- substr(as.character(classification), 1, 1)
  if (all.equal(levels(classification),
              c("++", "+", "+~", "~", "-~", "-", "--", "?+", "?-", "?"))) {
    simplified <- factor(simplified, levels = c("+", "-", "~", "?"))
    return(simplified)
  }
  if (all.equal(levels(classification), c("**", "*", "*~", "~", "?*", "?"))) {
    simplified <- factor(simplified, levels = c("*", "~", "?"))
    return(simplified)
  }
  stop("not a 10 scale classification")
}
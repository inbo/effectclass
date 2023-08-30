#' Use a Lower Scale Classification
#'
#' @description
#' `coarse_classification(y)` reduces the 10 scales from
#' `y <- classification(x)` to the 4 scales below.
#' - `+` **positive effect**: `reference < lcl`
#' - `~` **no effect**: `min(threshold) < lcl < reference` and
#' `reference < ucl < max(threshold)`
#' - `-` **negative effect**: `ucl < reference`
#' - `?` **unknown effect**: `lcl < min(threshold)` or `max(threshold) < ucl`
#'
#' `coarse_classification(y)` reduces the 6 scales from
#' `y <- remove_sign(classification(x))` into 3 scales.
#' @inheritParams remove_sign
#' @export
#' @importFrom assertthat assert_that
#' @family classification functions
coarse_classification <- function(classification) {
  is_effectclass(classification, message = "error")
  if (!attr(classification, "detailed")) {
    return(classification)
  }
  simplified <- substr(as.character(classification), 1, 1)
  if (attr(classification, "signed")) {
    structure(
      factor(simplified, levels = c("+", "~", "-", "?")),
      signed = TRUE,
      detailed = FALSE,
      class = c("effectclass", "factor")
    )
  } else {
    structure(
      factor(simplified, levels = c("*", "~", "?")),
      signed = FALSE,
      detailed = FALSE,
      class = c("effectclass", "factor")
    )
  }
}

#' Classify Effects by Comparing the Confidence Intervals with a Reference and
#' Thresholds
#' @param lcl A vector of lower confidence limits.
#' @param ucl A vector of upper confidence limits.
#' @param threshold A vector of either 1 or 2 thresholds.
#' A single threshold will be transformed into
#' `reference + c(-abs(threshold), abs(threshold))`.
#' @param reference The null hypothesis.
#' Defaults to 0.
#' @description
#' - `++` **strong positive effect**: `max(threshold) < lcl`
#' - `+` **positive effect**: `reference < lcl < max(threshold)` and
#' `max(threshold) < ucl`
#' - `+~` **moderate positive effect**: `reference < lcl` and
#' `ucl < max(threshold)`
#' - `~` **no effect**: `min(threshold) < lcl < reference` and
#' `reference < ucl < max(threshold)`
#' - `-~` **moderate negative effect**: `min(threshold) < lcl` and
#' `ucl < reference`
#' - `-` **negative effect**: `lcl < min(threshold)` and
#' `min(threshold) < ucl < reference`
#' - `--` **strong negative effect**: `ucl < min(threshold)`
#' - `?+` **potential positive effect**: `min(threshold) < lcl < reference` and
#' `max(threshold) < ucl`
#' - `?-` **potential negative effect**: `lcl < min(threshold)` and
#' `reference < ucl < max(threshold)`
#' - `?` **unknown effect**: `lcl < min(threshold)` and `max(threshold) < ucl`
#' @export
#' @importFrom assertthat assert_that is.number noNA
classification <- function(lcl, ucl, threshold, reference = 0) {
  assert_that(is.numeric(lcl), is.numeric(ucl), length(lcl) == length(ucl),
              is.numeric(threshold), noNA(threshold), is.number(reference),
              noNA(reference))
  if (length(threshold) == 1) {
    assert_that(-abs(threshold) < reference, reference < abs(threshold))
    threshold <- reference + c(-1, 1) * abs(threshold)
  } else {
    assert_that(length(threshold) == 2, min(threshold) < reference,
                reference < max(threshold))
    threshold <- sort(threshold)
  }
  classification <- ifelse(
    ucl < threshold[2],
    ifelse(
      ucl < reference,
      ifelse(ucl < threshold[1], "--", ifelse(lcl < threshold[1], "-", "-~")),
      ifelse(lcl > reference, "+~", ifelse(lcl < threshold[1], "?-", "~"))
    ),
    ifelse(
      lcl > reference,
      ifelse(lcl > threshold[2], "++", "+"),
      ifelse(lcl > threshold[1], "?+", "?")
    )
  )
  structure(
    factor(
      classification,
      levels = c("++", "+", "+~", "~", "-~", "-", "--", "?+", "?-", "?")
    ),
    signed = TRUE,
    detailed = TRUE,
    class = c("effectclass", "factor")
  )
}

#' Create `plotly`reference text
#' Returns a list text you can pass to the `annotations` argument of
#' `plotly::layout()`
#' @inheritParams classification
#' @param text A character vector with three elements with the text to display
#' on the reference line, bottom threshold line and upper threshold line.
#' Defaults to `c("reference", "important decrease", "important increase")`.
#' @param offset An numeric vector with the offset between `text` and the lines.
#' In units of the `y` variable.
#' Defaults to 10% of the difference between reference and threshold.
#' @template example_effect_data
#' @template example_effect_plotly
#' @family plotly add-ons
#' @export
#' @importFrom assertthat assert_that is.flag is.number is.string noNA
#' @export
reference_text <- function(
  threshold, reference = 0, offset,
  text = c("reference", "important decrease", "important increase")
) {
  assert_that(
    is.numeric(threshold), noNA(threshold), is.number(reference),
    noNA(reference), is.character(text), noNA(text), length(text) == 3
  )
  if (length(threshold) == 1) {
    threshold <- reference + c(-1, 1) * abs(threshold)
  } else {
    assert_that(
      length(threshold) == 2, min(threshold) < reference,
      reference < max(threshold)
    )
    threshold <- sort(threshold)
  }
  if (!missing(offset)) {
    assert_that(is.numeric(offset), noNA(offset), length(offset) == 3)
    offset <- offset + c(reference, threshold)
  } else {
    offset <- c(0.1 * threshold[2], 1.1 * threshold[1], 1.1 * threshold[2]) +
      0.9 * reference
  }
  list(
    x = 1, xref = "paper", y = offset, text = text,
    showarrow = FALSE
  )
}

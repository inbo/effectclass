#' Create `plotly`reference text
#' Returns a list text you can pass to the `annotations` argument of
#' `plotly::layout()`
#' @inheritParams classification
#' @template example_effect_data
#' @template example_effect_plotly
#' @family plotly add-ons
#' @export
#' @importFrom assertthat assert_that is.flag is.number is.string noNA
#' @export
reference_text <- function(
  threshold, reference = 0, offset = 0.1 * (reference + threshold),
  text = c("reference", "important decrease", "important increase")
) {
  assert_that(
    is.numeric(threshold), noNA(threshold), is.number(reference),
    noNA(reference), is.character(text), noNA(text), length(text) == 3,
    is.number(offset)
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
  list(
    x = 1, xref = "paper", y = offset + c(reference, threshold), text = text,
    showarrow = FALSE
  )
}

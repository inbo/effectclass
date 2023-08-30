#' Create `plotly`references
#' Returns a list shapes you can pass to the `shapes` argument of
#' `plotly::layout()`
#' @inheritParams classification
#' @param colour The colour for the references.
#' Defaults to `"black"`.
#' @param horizontal Display horizontal reference when `TRUE` (default).
#'  Display vertical reference when `FALSE`.
#' @template example_effect_data
#' @template example_effect_plotly
#' @family plotly add-ons
#' @export
#' @importFrom assertthat assert_that is.flag is.number is.string noNA
reference_shape <- function(
  threshold, reference = 0, colour = "black", horizontal = TRUE
) {
  assert_that(
    is.numeric(threshold), noNA(threshold), is.number(reference),
    noNA(reference), is.string(colour), noNA(colour), is.flag(horizontal),
    noNA(horizontal)
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
  if (horizontal) {
    ref_line <- list(
      type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = reference,
      y1 = reference, line = list(color = colour, dash = "longdash"),
      layer = "below"
    )
    threshold_line_up <- list(
      type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = threshold[1],
      y1 = threshold[1], line = list(color = colour, dash = "dot"),
      layer = "below"
    )
    threshold_line_down <- list(
      type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = threshold[2],
      y1 = threshold[2], line = list(color = colour, dash = "dot"),
      layer = "below"
    )
    return(list(ref_line, threshold_line_up, threshold_line_down))
  }
  ref_line <- list(
    type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = reference,
    x1 = reference, line = list(color = colour, dash = "longdash"),
    layer = "below"
  )
  threshold_line_up <- list(
    type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = threshold[1],
    x1 = threshold[1], line = list(color = colour, dash = "dot"),
    layer = "below"
  )
  threshold_line_down <- list(
    type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = threshold[2],
    x1 = threshold[2], line = list(color = colour, dash = "dot"),
    layer = "below"
  )
  list(ref_line, threshold_line_up, threshold_line_down)
}

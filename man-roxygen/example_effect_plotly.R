#' @examples
#' library(plotly)
#' plot_ly(z, x = ~x, y = ~estimate) |>
#'   add_fan(sd = ~sd, text = ~display) |>
#'   add_classification(lcl = ~lcl, ucl = ~ucl, threshold = 1) |>
#'   layout(
#'     hovermode = "x unified",
#'     shapes = reference_shape(threshold = 1),
#'     annotations = reference_text(threshold = 1)
#'   )
#' plot_ly(z, x = ~x, y = ~estimate) |>
#'   add_fan(sd = ~sd, step = 0.1, text = ~display) |>
#'   add_classification(
#'     lcl = ~lcl, ucl = ~ucl, threshold = 1, detailed = FALSE
#'    ) |>
#'   layout(
#'     shapes = reference_shape(threshold = 1, line = TRUE),
#'     annotations = reference_text(threshold = 1)
#'    )
#' plot_ly(z, x = ~x, y = ~estimate) |>
#'   add_fan(sd = ~sd, step = 0.2, hoverinfo = "none") |>
#'   add_classification(
#'     lcl = ~lcl, ucl = ~ucl, threshold = 1, signed = FALSE
#'   ) |>
#'   layout(shapes = reference_shape(threshold = 1))
#' plot_ly(z, x = ~x, y = ~estimate) |>
#'   add_fan(sd = ~sd, step = 0.3) |>
#'   add_classification(
#'     lcl = ~lcl, ucl = ~ucl, threshold = 1, detailed = FALSE, signed = FALSE
#'   ) |>
#'   layout(
#'     shapes = reference_shape(threshold = 1, line = TRUE)
#'   )
#'
#' # trend
#' plot_ly(data = trend, x = ~year, y = ~index) |>
#'   add_fan(sd = ~sd, text = ~display, hoverinfo = "text") |>
#'   add_classification(sd = ~sd, threshold = th) |>
#'   layout(
#'     hovermode = "x unified",
#'     shapes = reference_shape(threshold = th, reference = ref),
#'     annotations = reference_text(threshold = th, reference = ref)
#'   )

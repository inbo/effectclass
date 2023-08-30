#' @examples
#' library(plotly)
#' z$error_up <- z$ucl - z$estimate
#' z$error_down <- z$estimate - z$lcl
#' plot_ly(
#'   z, y = ~effect, x = ~estimate, type = "scatter", mode = "none",
#'   error_x = list(type = "data", array = ~error_up, arrayminus = ~error_down)
#' ) |>
#'   add_classification(lcl = ~lcl, ucl = ~ucl, threshold = 1) |>
#'   layout(
#'     shapes = reference_shape(threshold = 1, horizontal = FALSE)
#'   )
#' plot_ly(
#'   z, y = ~effect, x = ~estimate, type = "scatter", mode = "none",
#'   error_x = list(type = "data", array = ~error_up, arrayminus = ~error_down)
#' ) |>
#'   add_classification(
#'   lcl = ~lcl, ucl = ~ucl, threshold = 1, detailed = FALSE
#'  ) |>
#'   layout(
#'     shapes = reference_shape(threshold = 1, horizontal = FALSE)
#'   )
#' plot_ly(
#'   z, y = ~effect, x = ~estimate, type = "scatter", mode = "none",
#'   error_x = list(type = "data", array = ~error_up, arrayminus = ~error_down)
#' ) |>
#'   add_classification(
#'     lcl = ~lcl, ucl = ~ucl, threshold = 1, signed = FALSE
#'    ) |>
#'   layout(
#'     shapes = reference_shape(threshold = 1, horizontal = FALSE)
#'   )
#' plot_ly(
#'   z, y = ~effect, x = ~estimate, type = "scatter", mode = "none",
#'   error_x = list(type = "data", array = ~error_up, arrayminus = ~error_down)
#' ) |>
#'   add_classification(
#'     lcl = ~lcl, ucl = ~ucl, threshold = 1, detailed = FALSE, signed = FALSE
#'   ) |>
#'   layout(
#'     shapes = reference_shape(threshold = 1, horizontal = FALSE)
#'   )

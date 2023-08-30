#' @examples
#' library(plotly)
#' z$error_up <- z$ucl - z$estimate
#' z$error_down <- z$estimate - z$lcl
#' plot_ly(
#'   z, y = ~effect, x = ~estimate, type = "scatter", mode = "none",
#'   error_x = list(type = "data", array = ~error_up, arrayminus = ~error_down)
#' ) |>
#'   add_classification(lcl = ~lcl, ucl = ~ucl, threshold = 1)
#' plot_ly(
#'   z, y = ~effect, x = ~estimate, type = "scatter", mode = "none",
#'   error_x = list(type = "data", array = ~error_up, arrayminus = ~error_down)
#' ) |>
#'   add_classification(lcl = ~lcl, ucl = ~ucl, threshold = 1, detailed = FALSE)
#' plot_ly(
#'   z, y = ~effect, x = ~estimate, type = "scatter", mode = "none",
#'   error_x = list(type = "data", array = ~error_up, arrayminus = ~error_down)
#' ) |>
#'   add_classification(lcl = ~lcl, ucl = ~ucl, threshold = 1, signed = FALSE)
#' plot_ly(
#'   z, y = ~effect, x = ~estimate, type = "scatter", mode = "none",
#'   error_x = list(type = "data", array = ~error_up, arrayminus = ~error_down)
#' ) |>
#'   add_classification(
#'     lcl = ~lcl, ucl = ~ucl, threshold = 1, detailed = FALSE, signed = FALSE
#'   )

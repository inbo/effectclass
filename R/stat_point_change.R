#' Display points with classified change
#'
#' @inheritParams ggplot2::stat_bin
#' @inheritParams classification
#' @export
#' @importFrom ggplot2 layer
#' @examples
#' z <- data.frame(
#'   effect = factor(
#'     1:10,
#'     labels = c("unknown\neffect", "potential\npositive\neffect",
#'              "potential\nnegative\neffect", "no effect", "positive\neffect",
#'              "negative\neffect", "moderate\npositive\neffect",
#'              "moderate\nnegative\neffect", "strong\npositive\neffect",
#'              "strong\nnegative\neffect")
#'   ),
#'   estimate = c( 0,  0,    0,   0,   1,   -1,   0.5, -0.5, 1.5, -1.5),
#'   lcl =      c(-2, -0.9, -2,  -0.9, 0.1, -2,   0.1, -0.9, 1.1, -2),
#'   ucl =      c( 2,  2,    0.9, 0.9, 2,   -0.1, 0.9, -0.1, 2,   -1.1)
#' )
#' ggplot(z, aes(x = effect, y = estimate, ymin = lcl, ymax = ucl)) +
#'   geom_hline(yintercept = c(-1, 1, 0), linetype = c(3, 3, 2)) +
#'   geom_errorbar() +
#'   stat_point_change(threshold = 1, size = 3) +
#'   scale_shape_manual(values = c(17, 18, 16, 15, 5, 1), drop = FALSE) +
#'   coord_flip()

stat_point_change <- function(mapping = NULL, data = NULL,
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, ...,
                              threshold, reference = 0) {
  layer(
    stat = StatPointChange, data = data, mapping = mapping, geom = "point",
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(threshold = threshold, reference = reference, na.rm = na.rm,
                  ...)
  )
}

#' @importFrom ggplot2 ggproto Stat aes
StatPointChange <- ggproto(
  "StatPointChange", Stat,
  compute_group = function(data, scales, threshold, reference = 0) {
    data$classification <- classification(lcl = data$ymin, ucl = data$ymax,
                                          threshold = threshold,
                                          reference = reference)
    data$classification <- remove_sign(data$classification)
    return(data)
  },
  default_aes = aes(shape = stat(classification)),
  required_aes = c("x", "y", "ymin", "ymax")
)

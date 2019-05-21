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
#' library(ggplot2)
#' ggplot(z, aes(x = effect, y = estimate, ymin = lcl, ymax = ucl)) +
#'   geom_hline(yintercept = c(-1, 1, 0), linetype = c(3, 3, 2)) +
#'   geom_errorbar() +
#'   stat_point_change(threshold = 1, size = 3) +
#'   scale_point_change() +
#'   coord_flip()
#' ggplot(z[3:5, ], aes(x = effect, y = estimate, ymin = lcl, ymax = ucl)) +
#'   geom_hline(yintercept = c(-1, 1, 0), linetype = c(3, 3, 2)) +
#'   geom_errorbar() +
#'   stat_point_change(threshold = 1, size = 3) +
#'   scale_point_change() +
#'   coord_flip()
#' ggplot(z[3:5, ], aes(x = effect, y = estimate, ymin = lcl, ymax = ucl)) +
#'   geom_hline(yintercept = c(-1, 1, 0), linetype = c(3, 3, 2)) +
#'   geom_errorbar() +
#'   stat_point_change(threshold = 1, size = 3) +
#'   scale_point_change(drop = TRUE) +
#'   coord_flip()

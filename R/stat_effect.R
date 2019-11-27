#' Display points with classified effect
#'
#' @inheritParams ggplot2::stat_bin
#' @inheritParams classification
#' @export
#' @importFrom ggplot2 layer
#' @template example_effect
#' @family geom
stat_effect <- function(
  mapping = NULL, data = NULL, position = "identity", na.rm = FALSE, #nolint
  show.legend = NA, inherit.aes = TRUE, ..., threshold, reference = 0 #nolint
) {
  layer(
    stat = stateffect, data = data, mapping = mapping, geom = "point",
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(threshold = threshold, reference = reference, na.rm = na.rm,
                  ...)
  )
}

#' @importFrom ggplot2 ggproto Stat aes
stateffect <- ggproto(
  "StatEffectChange", Stat,
  compute_group = function(data, scales, threshold, reference = 0) {
    data$classification <- classification(lcl = data$ymin, ucl = data$ymax,
                                          threshold = threshold,
                                          reference = reference)
    data$classification <- remove_sign(data$classification)
    data$ymin <- NULL
    data$ymax <- NULL
    return(data)
  },
  default_aes = aes(shape = stat(classification)),
  required_aes = c("x", "y", "ymin", "ymax")
)

#' A scale for effect points
#' @inheritParams ggplot2::scale_shape_manual
#' @param drop Should unused factor levels be omitted from the scale?
#' The default, `FALSE`, uses all the levels in the factor;
#' `TRUE` uses the levels that appear in the data.
#' @param ... Arguments passed to \code{\link[ggplot2]{scale_shape_manual}} from
#' ggplot2.
#' @export
#' @importFrom ggplot2 scale_shape_manual
#' @template example_effect
scale_effect <- function(
  ..., values = c("**" = 17, "*~" = 18, "~" = 16, "*" = 15, "?*" = 5, "?" = 1),
  drop = FALSE
) {
  scale_shape_manual(..., values = values, drop = drop)
}

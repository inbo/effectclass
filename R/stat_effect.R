#' Display points with classified effect
#'
#' @inheritParams ggplot2::stat_bin
#' @inheritParams classification
#' @inheritParams scale_effect
#' @export
#' @importFrom ggplot2 layer
#' @template example_effect
#' @family ggplot2
stat_effect <- function(
  mapping = NULL, data = NULL, position = "identity", na.rm = FALSE, #nolint
  show.legend = NA, inherit.aes = TRUE, ..., threshold, reference = 0, #nolint
  detailed = TRUE, signed = TRUE
) {
  layer(
    stat = stateffect, data = data, mapping = mapping, geom = "point",
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(threshold = threshold, reference = reference, na.rm = na.rm,
                  detailed = detailed, signed = signed, ...)
  )
}

#' @importFrom ggplot2 ggproto Stat aes
stateffect <- ggproto(
  "StatEffectChange", Stat,
  compute_group = function(
    data, scales, threshold, reference = 0, detailed, signed
  ) {
    data$classification <- classification(lcl = data$ymin, ucl = data$ymax,
                                          threshold = threshold,
                                          reference = reference)
    if (!detailed) {
      data$classification <- coarse_classification(data$classification)
    }
    if (!signed) {
      data$classification <- remove_sign(data$classification)
    }
    data$ymin <- NULL
    data$ymax <- NULL
    return(data)
  },
  default_aes = aes(shape = stat(classification)),
  required_aes = c("x", "y", "ymin", "ymax")
)

#' A scale for effect points
#' @inheritParams ggplot2::scale_shape_manual
#'
#' @param detailed `TRUE` indicates a detailed \code{\link{classification}};
#' `FALSE` a \code{\link{coarse_classification}}.
#' Defaults to `TRUE`.
#' @param signed `TRUE` indicates a signed classification;
#' `FALSE` a classification with \code{\link{remove_sign}}.
#' Defaults to `TRUE`.
#' @param guide A \code{\link[ggplot2]{guide_legend}} specification for the
#' legend.
#' Defaults to `guide_legend(ncol = 2)`.
#' @param drop Should unused factor levels be omitted from the scale?
#' The default, `FALSE`, uses all the levels in the factor;
#' `TRUE` uses the levels that appear in the data.
#' @param ... Arguments passed to \code{\link[ggplot2]{scale_shape_manual}} from
#' `ggplot2`.
#' Note that `values` is set by `scale_effect()`.
#' @export
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom ggplot2 scale_shape_manual guide_legend
#' @template example_effect
#' @family ggplot2
scale_effect <- function(
  ..., detailed = TRUE, signed = TRUE, guide = guide_legend(ncol = 2),
  drop = FALSE
) {
  assert_that(is.flag(detailed), is.flag(signed), noNA(detailed), noNA(signed))
  values <- switch(
    paste0(detailed, signed),
    TRUETRUE = detailed_signed_palette,
    TRUEFALSE = detailed_unsigned_palette,
    FALSETRUE = coarse_signed_palette,
    FALSEFALSE = coarse_unsigned_palette
  )
  scale_shape_manual(..., values = values, guide = guide, drop = drop)
}

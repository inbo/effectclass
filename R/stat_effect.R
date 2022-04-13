#' Display points with classified effect
#'
#' @param shape_colour Colour the background of the labels according to the
#' classification.
#' @inheritParams ggplot2::stat_bin
#' @inheritParams classification
#' @inheritParams scale_effect
#' @export
#' @importFrom assertthat assert_that has_name is.flag noNA
#' @importFrom ggplot2 layer
#' @template example_effect
#' @family ggplot2
#' @seealso \code{\link{classification}}
stat_effect <- function(
  mapping = NULL, data = NULL, position = "identity",
  na.rm = FALSE, show.legend = NA, # nolint: object_name_linter.
  inherit.aes = TRUE, # nolint: object_name_linter.
  ..., threshold, reference = 0, detailed = TRUE, signed = TRUE,
  shape_colour = TRUE
) {
  assert_that(is.flag(shape_colour), noNA(shape_colour))
  if (shape_colour) {
    scale_layer <- scale_effect(
      ..., detailed = detailed, signed = signed
    )
    dots <- list(...)
    text_layer <- layer(
      stat = stateffect_fill, data = data, mapping = mapping, geom = "label",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        threshold = threshold, reference = reference, na.rm = na.rm,
        detailed = detailed, signed = signed,
        colour = ifelse(has_name(dots, "colour"), dots$colour, "white")
      )
    )
    return(list(text_layer, scale_layer))
  }
  layer(
    stat = stateffect, data = data, mapping = mapping, geom = "label",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      threshold = threshold, reference = reference, na.rm = na.rm,
      detailed = detailed, signed = signed
    )
  )
}

#' @importFrom assertthat assert_that is.flag noNA
compute_group_stateffect <- function(
  data, scales, threshold, reference = 0, detailed = TRUE, signed = TRUE, ...
) {
  assert_that(is.flag(detailed), noNA(detailed))
  assert_that(is.flag(signed), noNA(signed))
  data$classification <- classification(
    lcl = data$ymin, ucl = data$ymax, threshold = threshold,
    reference = reference
  )
  if (!detailed) {
    data$classification <- coarse_classification(data$classification)
  }
  if (!signed) {
    data$classification <- remove_sign(data$classification)
  }
  return(data)
}

#' @importFrom ggplot2 ggproto Stat aes
stateffect <- ggproto(
  "StatEffectChange", Stat, compute_group = compute_group_stateffect,
  default_aes = aes(label = stat(classification)),
  required_aes = c("x", "y", "ymin", "ymax")
)

#' @importFrom ggplot2 ggproto Stat aes
stateffect_fill <- ggproto(
  "StatEffectChange", Stat, compute_group = compute_group_stateffect,
  default_aes = aes(label = stat(classification), fill = stat(classification)),
  required_aes = c("x", "y", "ymin", "ymax")
)

#' A scale for effect points
#' @inheritDotParams ggplot2::scale_shape_manual
#'
#' @param detailed `TRUE` indicates a detailed \code{\link{classification}};
#' `FALSE` a \code{\link{coarse_classification}}.
#' Defaults to `TRUE`.
#' @param signed `TRUE` indicates a signed classification;
#' `FALSE` a classification with \code{\link{remove_sign}}.
#' Defaults to `TRUE`.
#' @param ncol the number of columns in the legend.
#' Default to 1.
#' @param drop Should unused factor levels be omitted from the scale?
#' The default, `FALSE`, uses all the levels in the factor;
#' `TRUE` uses the levels that appear in the data.
#' @param ... Arguments passed to \code{\link[ggplot2]{scale_shape_manual}} from
#' `ggplot2`.
#' Note that `values` is set by `scale_effect()`.
#' @export
#' @importFrom assertthat assert_that has_name is.count is.flag noNA
#' @importFrom ggplot2 guide_legend scale_fill_manual
#' @template example_effect
#' @family ggplot2
scale_effect <- function(
  ..., detailed = TRUE, signed = TRUE, ncol = 1, drop = FALSE
) {
  assert_that(
    is.flag(detailed), is.flag(signed), noNA(detailed), noNA(signed),
    is.count(ncol)
  )
  dots <- list(...)
  if (has_name(dots, "values")) {
    values <- dots[["values"]]
  } else {
    values <- switch(
      paste0(detailed, signed),
      TRUETRUE = detailed_signed_palette,
      TRUEFALSE = detailed_unsigned_palette,
      FALSETRUE = coarse_signed_palette,
      FALSEFALSE = coarse_unsigned_palette
    )
  }
  guide <- guide_legend(ncol = ncol, override.aes = list(label = names(values)))
  scale_fill_manual(..., values = values, guide = guide, drop = drop)
}

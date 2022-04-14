#' Display points with classified effect
#'
#' @param shape_colour Colour the background of the labels according to the
#' classification.
#' Defaults to `TRUE`.
#' @param errorbar Display the uncertainty as error bars.
#' Defaults to `TRUE`.
#' @param error_colour Colour the error bars accorings to the classification.
#' Defaults to `TRUE`.
#' @inheritParams ggplot2::stat_bin
#' @inheritParams classification
#' @inheritParams scale_effect
#' @export
#' @importFrom assertthat assert_that has_name is.flag noNA
#' @importFrom ggplot2 geom_errorbar layer
#' @importFrom grid unit
#' @template example_effect
#' @family ggplot2
#' @seealso \code{\link{classification}}
stat_effect <- function(
  mapping = NULL, data = NULL, position = "identity",
  na.rm = FALSE, show.legend = NA, # nolint: object_name_linter.
  inherit.aes = TRUE, # nolint: object_name_linter.
  ..., threshold, reference = 0, detailed = TRUE, signed = TRUE,
  shape_colour = TRUE, error = TRUE, error_colour = TRUE
) {
  assert_that(is.flag(shape_colour), noNA(shape_colour))
  assert_that(is.flag(error), noNA(error))
  dots <- list(...)
  if (error) {
    assert_that(is.flag(error_colour), noNA(error_colour))
    if (error_colour) {
      error_layer <- layer(
          stat = stateffect_colour, data = data, mapping = mapping,
          geom = "errorbar", position = position, show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(
            threshold = threshold, reference = reference, na.rm = na.rm,
            detailed = detailed, signed = signed
          )
        )
    } else {
      error_layer <- geom_errorbar(
        mapping = mapping, data = data, position = position, na.rm = na.rm,
        show.legend = show.legend, inherit.aes = inherit.aes
      )
    }
  } else {
    error_layer <- NULL
  }
  if (shape_colour) {
    text_layer <- layer(
      stat = stateffect_fill, data = data, mapping = mapping, geom = "label",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        threshold = threshold, reference = reference, na.rm = na.rm,
        detailed = detailed, signed = signed,
        colour = ifelse(has_name(dots, "colour"), dots$colour, "white"),
        label.r = unit(0.5, "lines")
      )
    )
  } else {
    text_layer <- layer(
      stat = stateffect, data = data, mapping = mapping, geom = "label",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        threshold = threshold, reference = reference, na.rm = na.rm,
        detailed = detailed, signed = signed, label.r = unit(0.5, "lines")
      )
    )
  }
  scale_layer <- scale_effect(
    ..., detailed = detailed, signed = signed, fill = shape_colour,
    colour = error_colour
  )
  list(error_layer, text_layer, scale_layer)
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

#' @importFrom ggplot2 ggproto Stat aes
stateffect_colour <- ggproto(
  "StatEffectChange", Stat, compute_group = compute_group_stateffect,
  default_aes = aes(colour = stat(classification)),
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
#' @param colour return \code{\link[ggplot2]{scale_colour_manual}}
#' @param fill return \code{\link[ggplot2]{scale_fill_manual}}
#' @param ... Arguments passed to \code{\link[ggplot2]{scale_colour_manual}} and
#' \code{\link[ggplot2]{scale_fill_manual}}.
#' Note that `values` is set by `scale_effect()`.
#' @export
#' @importFrom assertthat assert_that has_name is.count is.flag noNA
#' @importFrom ggplot2 guide_legend scale_colour_manual scale_fill_manual
#' @template example_effect
#' @family ggplot2
scale_effect <- function(
  ..., detailed = TRUE, signed = TRUE, fill = TRUE,
  colour = TRUE
) {
  assert_that(
    is.flag(detailed), is.flag(signed), is.flag(fill), is.flag(colour),
    noNA(detailed), noNA(signed), noNA(fill), noNA(colour)
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
  guide <- guide_legend(override.aes = list(label = names(values)))
  if (fill) {
    scales <- scale_fill_manual(..., values = values, guide = guide)
  } else {
    scales <- NULL
  }
  if (colour) {
    scales <- list(scales, scale_colour_manual(..., values = values))
  }
  return(scales)
}

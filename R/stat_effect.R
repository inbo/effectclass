#' Display points with classified effect
#'
#' @param shape_colour Colour the background of the labels according to the
#' classification.
#' Defaults to `TRUE`.
#' @param errorbar Display the uncertainty as error bars.
#' Defaults to `TRUE`.
#' @param error_colour Colour the error bars according to the classification.
#' Defaults to `TRUE`.
#' @param size Size of the symbols.
#' @param ref_line Which reference lines to display.
#' `"all"` displays a dashed horizontal line at the `reference` and a dotted
#' horizontal line at the `threshold`.
#' `"ref"` displays a dashed horizontal line at the `reference`.
#' `"non`e"` displays no horizontal lines.
#' @inheritParams ggplot2::stat_bin
#' @inheritParams classification
#' @inheritParams scale_effect
#' @export
#' @importFrom assertthat assert_that has_name is.flag noNA
#' @importFrom ggplot2 geom_errorbar geom_hline layer scale_colour_manual
#' scale_fill_manual
#' @importFrom grid unit
#' @template example_effect
#' @family ggplot2
#' @seealso \code{\link{classification}}
stat_effect <- function(
  mapping = NULL, data = NULL, position = "identity",
  na.rm = FALSE, show.legend = NA, # nolint: object_name_linter.
  inherit.aes = TRUE, # nolint: object_name_linter.
  ..., threshold, reference = 0, detailed = TRUE, signed = TRUE,
  shape_colour = TRUE, errorbar = TRUE, error_colour = TRUE, size = 6,
  labels = class_labels(lang = "en", detailed = detailed, signed = signed),
  ref_line = c("all", "ref", "none")
) {
  assert_that(is.flag(shape_colour), noNA(shape_colour))
  assert_that(is.flag(errorbar), noNA(errorbar))
  ref_line <- match.arg(ref_line)
  dots <- list(...)
  if (errorbar) {
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
    point_layer <- layer(
      stat = stateffect_colour, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        threshold = threshold, reference = reference, na.rm = na.rm,
        detailed = detailed, signed = signed, size = size, error = errorbar
      )
    )
  } else {
    point_layer <- layer(
      stat = stateffect, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        threshold = threshold, reference = reference, na.rm = na.rm,
        detailed = detailed, signed = signed, size = size, error = errorbar
      )
    )
  }
  text_layer <- layer(
    stat = stateffect, data = data, mapping = mapping, geom = "text",
    position = position, show.legend = TRUE, inherit.aes = inherit.aes,
    params = list(
      threshold = threshold, reference = reference,
      detailed = detailed, signed = signed, size = 0.6 * size, na.rm = na.rm,
      colour = ifelse(has_name(dots, "colour"), dots$colour, "white"),
      error = errorbar
    )
  )
  scale_layer <- scale_effect(
    ..., detailed = detailed, signed = signed, fill = FALSE,
    colour = shape_colour || (errorbar && error_colour), labels = labels
  )
  if (ref_line == "all" && length(threshold) == 1) {
    threshold <- reference + c(-1, 1) * abs(threshold)
  }
  ref_layer <- switch(
    ref_line,
    "all" = list(
      geom_hline(yintercept = reference, linetype = 2),
      geom_hline(yintercept = threshold, linetype = 3)
    ),
    "ref" = geom_hline(yintercept = reference, linetype = 2),
    list()
  )
  list(ref_layer, error_layer, point_layer, text_layer, scale_layer)
}

#' @importFrom assertthat assert_that is.flag noNA
compute_group_stateffect <- function(
  data, scales, threshold, reference = 0, detailed = TRUE, signed = TRUE,
  error = TRUE, ...
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
  if (!error) {
    data$ymin <- NULL
    data$ymax <- NULL
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
stateffect_colour <- ggproto(
  "StatEffectChange", Stat, compute_group = compute_group_stateffect,
  default_aes = aes(colour = stat(classification)),
  required_aes = c("x", "y", "ymin", "ymax")
)

#' A scale for effect points
#' @inheritDotParams ggplot2::scale_shape_manual
#'
#' @param detailed `TRUE` indicates a detailed [classification()];
#' `FALSE` a [coarse_classification()].
#' Defaults to `TRUE`.
#' @param signed `TRUE` indicates a signed classification;
#' `FALSE` a classification with [remove_sign()].
#' Defaults to `TRUE`.
#' @param colour return [ggplot2::scale_colour_manual()]
#' @param fill return [ggplot2::scale_fill_manual()]
#' @param labels the labels for the legend.
#' @param drop Drop unused levels.
#' This is always `FALSE`.
#' Changing this argument has no effect.
#' We provide the argument to avoid errors in case the user sets the argument.
#' @param ... Arguments passed to [ggplot2::scale_colour_manual()] and
#' [ggplot2::scale_fill_manual()].
#' Note that `values` and `drop` are set by `scale_effect()`.
#' @export
#' @importFrom assertthat assert_that has_name is.count is.flag noNA
#' @importFrom ggplot2 guide_legend scale_colour_manual scale_fill_manual
#' @template example_effect
#' @family ggplot2
scale_effect <- function(
  ..., detailed = TRUE, signed = TRUE, fill = TRUE, colour = TRUE, drop = FALSE,
  labels = class_labels(lang = "en", detailed = detailed, signed = signed)
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
    scales <- scale_fill_manual(
      ..., values = values, guide = guide, labels = labels, drop = FALSE
    )
  } else {
    scales <- NULL
  }
  if (colour) {
    scales <- list(
      scales,
      scale_colour_manual(
        ..., guide = guide, values = values, labels = labels, drop = FALSE
      )
    )
  }
  return(scales)
}

#' Return a standardised set of labels for the classification
#' @param type What type of effect.
#' Currently available are `"trend"` and `"effect"`.
#' @param lang The language.
#' Currently available are `"en"` (English) and `"nl"` (Dutch).
#' Defaults to `"en"`.
#' Please contact the maintainer if you have suggestions for more languages.
#' @inheritParams scale_effect
#' @export
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom stats setNames
#' @family display
class_labels <- function(
  type = c("trend", "effect"), lang = c("en", "nl"), detailed = TRUE,
  signed = TRUE
) {
  assert_that(is.flag(detailed), noNA(detailed), is.flag(signed), noNA(signed))
  type <- match.arg(type)
  lang <- match.arg(lang)
  labels <- data.frame(
    symbol = rep(
      c(
        "++", "+", "+~", "~", "-~", "-", "--", "?+", "?-", "?", "**", "*", "*~",
        "?*"
      ), 2 * 2
    ),
    order = rep(c(1:10, 1:3, 5), 2 * 2),
    label = c(
      # en trend
      "strong increase", "increase", "moderate increase", "stable",
      "moderate decrease",  "decrease", "strong decrease",
      "potential increase", "potential decrease", "unknown", "strong trend",
      "trend", "moderate trend", "potential trend",
      # en effect
      "strong positive effect", "positive effect", "moderate positive effect",
      "no effect", "moderate negative effect", "negative effect",
      "strong negative effect", "potential positive effect",
      "potential negative effect", "unknown effect", "strong effect", "effect",
      "moderate effect", "potential effect",
      # nl trend
      "sterke toename", "toename", "matige toename", "stabiel",
      "matige daling",  "daling", "sterke daling", "mogelijke toename",
      "mogelijke daling", "onduidelijke trend", "sterke trend", "trend",
      "matige trend", "mogelijke trend",
      # nl effect
      "sterke positief effect", "positief effect", "matige positief effect",
      "geen effect", "matige negatief effect",  "negatief effect",
      "sterk negatief effect", "mogelijke positief effect",
      "mogelijk negatief effect", "onduidelijke effect", "sterk effect",
      "effect", "matige effect", "mogelijke effect"
    ),
    lang = rep(c("en", "nl"), each = 2 * 14),
    type = rep(c("trend", "effect"), each = 14)
  )
  labels <- labels[labels$type == type & labels$lang == lang, ]
  labels <- labels[!grepl(ifelse(signed, "\\*", "[\\+\\-]"), labels$symbol), ]
  labels <- labels[nchar(labels$symbol) <= detailed + 1, ]
  labels <- labels[order(labels$order), ]
  setNames(labels$label, labels$symbol)
}

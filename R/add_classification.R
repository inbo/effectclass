#' Add a point symbol with classification to a `plotly` object
#'
#' See `classification()` for an explication on how the classification is done.
#' @inheritParams plotly::add_trace
#' @inheritParams add_fan
#' @inheritParams classification
#' @inheritParams stat_effect
#' @param prob The coverage of the confidence interval when calculated from the
#' mean `y` and standard error `sd`.
#' Note that the function assumes a normal distribution at the `link` scale.
#' @param size Size of the point symbol.
#' @param labels a vector of labels for the classification hover information.
#' See `class_labels()` for inspiration.
#' @family plotly add-ons
#' @template example_effect_data
#' @template example_effect_plotly
#' @export
#' @importFrom assertthat assert_that has_name is.flag is.number noNA
#' @importFrom plotly add_markers add_text
#' @importFrom stats qnorm
add_classification <- function(
  p, x = NULL, y = NULL, ..., data = NULL, inherit = TRUE, sd, lcl = NULL,
  ucl = NULL, threshold, reference = 0, prob = 0.95, size = 20,
  link = c("identity", "log", "logit"), detailed = TRUE, signed = TRUE,
  labels = class_labels(lang = "en", detailed = detailed, signed = signed),
  text = NULL, hoverinfo = "text"
) {
  assert_that(
    is.flag(inherit), noNA(inherit), is.flag(detailed), noNA(detailed),
    is.flag(signed), noNA(signed)
  )
  link <- match.arg(link)
  if (inherit) {
    x <- coalesce(x, p$x$attrs[[1]][["x"]])
    y <- coalesce(y, p$x$attrs[[1]][["y"]])
    text <- coalesce(text, p$x$attrs[[1]][["text"]])
    data <- coalesce(data, p$x$visdat[[1]]())
  }
  stopifnot(
    "Please provide `x`, `y` and `data`" =
      !is.null(x) && !is.null(y) && !is.null(data)
  )
  assert_that(
    inherits(data, "data.frame"), has_name(data, as.character(x[[2]]))
  )
  if (is.null(lcl) || is.null(ucl)) {
    assert_that(
      inherits(sd, "formula"), has_name(data, as.character(c(y[[2]], sd[[2]]))),
      is.number(prob), 0 < prob, prob < 1
    )
    y_1 <- data[[y[[2]]]]
    y_1 <- switch(link, identity = y_1, log = log(y_1), logit = qlogis(y_1))
    sd_1 <- data[[sd[[2]]]]
    lcl_1 <- qnorm(0.5 - prob / 2, mean = y_1, sd = sd_1)
    ucl_1 <- qnorm(0.5 + prob / 2, mean = y_1, sd = sd_1)
  } else {
    assert_that(
      inherits(lcl, "formula"), inherits(ucl, "formula"),
      has_name(data, as.character(c(lcl[[2]], ucl[[2]])))
    )
    lcl_1 <- data[[lcl[[2]]]]
    ucl_1 <- data[[ucl[[2]]]]
    lcl_1 <- switch(
      link, identity = lcl_1, log = log(lcl_1), logit = qlogis(lcl_1)
    )
    ucl_1 <- switch(
      link, identity = ucl_1, log = log(ucl_1), logit = qlogis(ucl_1)
    )
  }
  data$classification <- classification(
    lcl = lcl_1, ucl = ucl_1, threshold = threshold, reference = reference
  )
  if (!detailed) {
    data$classification <- coarse_classification(data$classification)
  }
  if (!signed) {
    data$classification <- remove_sign(data$classification)
  }
  if (is.null(text)) {
    assert_that(
      all(levels(data$classification) %in% names(labels)),
      msg = sprintf(
        "`label` must have each of these names: %s",
        paste(levels(data$classification), sep = ", ")
      )
    )
    text <- ~hoverinfo
    data$hoverinfo <- labels[as.character(data$classification)]
  }
  marker_color <- switch(
    paste0(detailed, signed),
    TRUETRUE = detailed_signed_palette, TRUEFALSE = detailed_unsigned_palette,
    FALSETRUE = coarse_signed_palette, FALSEFALSE = coarse_unsigned_palette
  )
  for (i in sort(unique(data$classification))) {
    p |>
      add_markers(
        x = x, y = y, text = text, hoverinfo = hoverinfo,
        data = data[data$classification == i, ],
        inherit = FALSE, showlegend = FALSE,
        marker = list(size = size, color = marker_color[i])
      ) |>
      add_text(
        x = x, y = y, text = i, data = data[data$classification == i, ],
        inherit = FALSE, showlegend = FALSE, hoverinfo = "none",
        textfont = list(size = 0.6 * size, color = "white")
      ) -> p
  }
  p
}

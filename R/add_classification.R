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
#' @param ref_label The label for the reference point.
#' Will be used for the points where `is.na(sd)` or both `is.na(lcl)` and
#' `is.na(ucl)`.
#' @param ref_colour The colour for the reference point.
#' @family plotly add-ons
#' @template example_effect_data
#' @template example_effect_plotly
#' @export
#' @importFrom assertthat assert_that has_name is.flag is.number noNA
#' @importFrom plotly add_markers add_text
#' @importFrom stats qnorm
add_classification <- function(
  p, x = NULL, y = NULL, ..., data = NULL, inherit = TRUE, sd, lcl = NULL,
  ucl = NULL, threshold, reference = 0, prob = 0.9, size = 20,
  link = c("identity", "log", "logit"), detailed = TRUE, signed = TRUE,
  labels = class_labels(lang = "en", detailed = detailed, signed = signed),
  text = NULL, hoverinfo = "text", ref_label = "reference",
  ref_colour = "#C04384"
) {
  assert_that(
    is.flag(inherit), noNA(inherit), is.flag(detailed), noNA(detailed),
    is.flag(signed), noNA(signed), is.string(ref_label), is.string(ref_colour)
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
  if (inherits(data, "SharedData")) {
    stopifnot(requireNamespace("crosstalk", quietly = TRUE))
    df <- data$origData()
  } else {
    assert_that(inherits(data, "data.frame"))
    df <- data
  }
  assert_that(has_name(df, as.character(x[[2]])))
  if (is.null(lcl) || is.null(ucl)) {
    assert_that(
      inherits(sd, "formula"), has_name(df, as.character(c(y[[2]], sd[[2]]))),
      is.number(prob), 0 < prob, prob < 1
    )
    y_1 <- df[[y[[2]]]]
    y_1 <- switch(link, identity = y_1, log = log(y_1), logit = qlogis(y_1))
    sd_1 <- df[[sd[[2]]]]
    lcl_1 <- qnorm(0.5 - prob / 2, mean = y_1, sd = sd_1)
    ucl_1 <- qnorm(0.5 + prob / 2, mean = y_1, sd = sd_1)
  } else {
    assert_that(
      inherits(lcl, "formula"), inherits(ucl, "formula"),
      has_name(df, as.character(c(lcl[[2]], ucl[[2]])))
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
  ref <- is.na(lcl_1) & is.na(ucl_1)
  lcl_1[ref] <- ucl_1[ref] <- reference
  df$classification <- classification(
    lcl = lcl_1, ucl = ucl_1, threshold = threshold, reference = reference
  )
  if (!detailed) {
    df$classification <- coarse_classification(df$classification)
  }
  if (!signed) {
    df$classification <- remove_sign(df$classification)
  }
  if (is.null(text)) {
    assert_that(
      all(levels(df$classification) %in% names(labels)),
      msg = sprintf(
        "`label` must have each of these names: %s",
        paste(levels(df$classification), sep = ", ")
      )
    )
    text <- ~hoverinfo
    df$hoverinfo <- labels[as.character(df$classification)]
    df$hoverinfo[ref] <- ref_label
  }
  switch(
    paste0(detailed, signed),
    TRUETRUE = detailed_signed_palette, TRUEFALSE = detailed_unsigned_palette,
    FALSETRUE = coarse_signed_palette, FALSEFALSE = coarse_unsigned_palette
  ) |>
    c(R = ref_colour) -> marker_color
  df$classification <- as.character(df$classification)
  df$classification[ref] <- "R"
  for (i in sort(unique(df$classification))) {
    this_data <- df[df$classification == i, ]
    if (inherits(data, "SharedData")) {
      this_data <- crosstalk::SharedData$new(
        data = this_data, group = data$groupName(),
        key = data$key()[df$classification == i]
      )
    }
    p |>
      add_markers(
        x = x, y = y, text = text, hoverinfo = hoverinfo, data = this_data,
        inherit = TRUE, showlegend = FALSE,
        marker = list(size = size, color = marker_color[i]), ...
      ) |>
      add_text(
        x = x, y = y, text = i, data = this_data,
        inherit = TRUE, showlegend = FALSE, hoverinfo = "none",
        textfont = list(size = 0.6 * size, color = "white"), ...
      ) -> p
  }
  p
}

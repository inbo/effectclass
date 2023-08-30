#' Add a point symbol with classification
#' @inheritParams plotly::add_trace
#' @inheritParams add_fan
#' @inheritParams classification
#' @param prob The coverage of the confidence interval when calculated from the
#' mean `y` and standard error `sd`.
#' @param size Size of the point symbol.
#' @family plotly
#' @export
#' @importFrom assertthat assert_that has_name is.flag is.number noNA
#' @importFrom plotly add_trace
#' @importFrom stats qnorm
add_classification <- function(
  p, x = NULL, y = NULL, ..., data = NULL, inherit = TRUE, sd, lcl, ucl,
  threshold, reference = 0, prob = 0.95, link = c("identity", "log", "logit"),
  size = 20
) {
  assert_that(is.flag(inherit), noNA(inherit))
  link <- match.arg(link)
  if (inherit) {
    x <- coalesce(x, p$x$attrs[[1]][["x"]])
    y <- coalesce(y, p$x$attrs[[1]][["y"]])
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
  p |>
    add_trace(
      x = x, y = y, color = ~classification, text = ~classification,
      data = data, inherit = FALSE, showlegend = FALSE,
      type = "scatter", mode = "markers+text",
      marker = list(size = size, color = detailed_signed_palette),
      textfont = list(size = 0.6 * size, color = "white")
    )
}

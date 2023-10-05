#' Add a fan plot to a `plotly` object
#'
#' A fan plot consist of a set of transparent ribbons each representing a
#' different coverage of the uncertainty around an estimate.
#' The coverages are based on the assumption of a normal distribution with mean
#' `link(y)` and standard error `sd`.
#' @inheritParams plotly::add_polygons
#' @param y the variable median on the natural scale.
#' @param sd the variable of the standard error on the link scale.
#' @param link the link between the natural scale and the link scale.
#' Defaults to `"identity"`.
#' @param max_prob The coverage of the widest band.
#' Defaults to `0.9`.
#' @param step the step size between consecutive bands.
#' The function adds all bands with coverage `max_prob - i * step` for all
#' positive integer values `i` resulting in a positive coverage.
#' Defaults to `0.05`.
#' @param fillcolor The fill colour of the fan.
#' Defaults to a greyish blue.
#' @param hoverinfo Which hover information to display.
#' Defaults to `"text"`.
#' When no `"text"` variable is specified, the function displays a formatted
#' confidence interval.
#' @template example_effect_data
#' @template example_effect_plotly
#' @family plotly add-ons
#' @export
#' @importFrom assertthat assert_that is.flag is.number is.string noNA
#' @importFrom plotly add_ribbons
add_fan <- function(
  p, x = NULL, y = NULL, ..., sd, link = c("identity", "log", "logit"),
  max_prob = 0.9, step = 0.05, fillcolor = coarse_unsigned_palette[2],
  data = NULL, inherit = TRUE, text = NULL, hoverinfo = "text"
) {
  assert_that(
    is.flag(inherit), noNA(inherit), is.string(fillcolor), noNA(fillcolor),
    is.number(max_prob), 0 < max_prob, max_prob < 1,
    is.number(step), noNA(step), 0 < step, step <= max_prob
  )
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
  dots <- list(...)
  if (is.null(text)) {
    text <- ~hoverinfo
  }
  for (prob in seq(max_prob, 1e-6, by = -step)) {
    dots$hoverinfo <- ifelse(prob < max_prob, "none", hoverinfo)
    dots$x <- x
    dots$text <- text
    dots$ymin <- ~lcl
    dots$ymax <- ~ucl
    dots$showlegend <- FALSE
    dots$opacity <- 1 - prob / (prob + step)
    dots$inherit <- TRUE
    dots$line <- list(width = 0)
    dots$fillcolor <- fillcolor
    dots$p <- p
    dots$data <- error_ribbon(
      data = data, y = y, sd = sd, prob = prob, link = link
    )
    p <- do.call(add_ribbons, dots)
  }
  p
}

coalesce <- function(x, y) {
  if (length(x) > 0 || is_blank(x)) {
    return(x)
  }
  y
}

is_blank <- function(x) {
  inherits(x, "element_blank") && inherits(x, "element")
}

#' @importFrom assertthat assert_that has_name is.number
#' @importFrom stats plogis qlogis qnorm
error_ribbon <- function(
  data, y, sd, prob = 0.95, link = c("identity", "log", "logit")
) {
  if (inherits(data, "SharedData")) {
    df <- data$origData()
  } else {
    assert_that(inherits(data, "data.frame"))
    df <- data
  }
  assert_that(
    inherits(y, "formula"), inherits(sd, "formula"), is.number(prob), 0 < prob,
    prob < 1, has_name(df, as.character(c(y[[2]], sd[[2]])))
  )
  stopifnot(
    "`y` is not numeric" = is.numeric(df[[y[[2]]]]),
    "`sd` is not numeric" = is.numeric(df[[sd[[2]]]])
  )
  link <- match.arg(link)
  y_1 <- df[[y[[2]]]]
  sd_1 <- df[[sd[[2]]]]
  y_1 <- switch(link, identity = y_1, log = log(y_1), logit = qlogis(y_1))
  lcl <- qnorm(0.5 - prob / 2, mean = y_1, sd = sd_1)
  ucl <- qnorm(0.5 + prob / 2, mean = y_1, sd = sd_1)
  df$lcl <- switch(link, identity = lcl, log = exp(lcl), logit = plogis(lcl))
  df$ucl <- switch(link, identity = ucl, log = exp(ucl), logit = plogis(ucl))
  ref <- is.na(sd_1)
  df$lcl[ref] <- y_1[ref]
  df$ucl[ref] <- y_1[ref]
  df$hoverinfo <- format_ci(df[[y[[2]]]], lcl = df$lcl, ucl = df$ucl)
  df$hoverinfo[ref] <- ""
  if (!inherits(data, "SharedData")) {
    return(df)
  }
  SharedData$new(data = df, group = data$groupName(), key = data$key())
}

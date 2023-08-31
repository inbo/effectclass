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
#' @examples
#' set.seed(20191218)
#' z <- data.frame(
#'   year = 1990:2019, dx = rnorm(30, sd = 0.2), s = rnorm(30, 0.5, 0.01)
#' )
#' z$index <- 3 + cumsum(z$dx)
#' library(plotly)
#' p <- plot_ly(z, x = ~year, y = ~index)
#' p |>
#'   add_fan(sd = ~s)
#' p |>
#'   add_fan(sd = ~s, step = 0.3, fillcolor = "green")
#' p |>
#'   add_fan(sd = ~s, max_prob = 0.95, link = "log")
#' @family plotly add-ons
#' @export
#' @importFrom assertthat assert_that is.flag is.number noNA
#' @importFrom plotly add_polygons
add_fan <- function(
  p, x = NULL, y = NULL, ..., sd, link = c("identity", "log", "logit"),
  max_prob = 0.9, step = 0.05, fillcolor = "blue", data = NULL, inherit = TRUE
) {
  assert_that(
    is.flag(inherit), noNA(inherit),
    is.number(max_prob), 0 < max_prob, max_prob < 1,
    is.number(step), noNA(step), 0 < step, step <= max_prob
  )
  if (inherit) {
    x <- coalesce(x, p$x$attrs[[1]][["x"]])
    y <- coalesce(y, p$x$attrs[[1]][["y"]])
    data <- coalesce(data, p$x$visdat[[1]]())
  }
  stopifnot(
    "Please provide `x`, `y` and `data`" =
      !is.null(x) && !is.null(y) && !is.null(data)
  )
  for (prob in seq(max_prob, 1e-6, by = -step)) {
    p |>
      add_polygons(
        x = x, y = y, showlegend = FALSE, opacity = step, inherit = FALSE,
        line = list(width = 0), fillcolor = fillcolor, ...,
        data = error_ribbon(
          data = data, x = x, y = y, sd = sd, prob = prob, link = link
        )
      ) -> p
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
    data, x, y, sd, prob = 0.95, link = c("identity", "log", "logit")
) {
  assert_that(
    inherits(data, "data.frame"), inherits(x, "formula"),
    inherits(y, "formula"), inherits(sd, "formula"),
    has_name(data, as.character(c(x[[2]], y[[2]], sd[[2]]))),
    is.number(prob), 0 < prob, prob < 1
  )
  link <- match.arg(link)
  x_1 <- data[[x[[2]]]]
  y_1 <- data[[y[[2]]]]
  sd_1 <- data[[sd[[2]]]]
  y_1 <- switch(link, identity = y_1, log = log(y_1), logit = qlogis(y_1))
  x_2 <- c(x_1, rev(x_1))
  y_2 <- c(
    qnorm(0.5 + prob / 2, mean = y_1, sd = sd_1),
    qnorm(0.5 - prob / 2, mean = rev(y_1), sd = rev(sd_1))
  )
  y_2 <- switch(link, identity = y_2, log = exp(y_2), logit = plogis(y_2))
  data.frame(x = x_2, y = y_2, prob = prob) |>
    `colnames<-`(as.character(c(x[[2]], y[[2]], "prob")))
}

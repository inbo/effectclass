#' Display a fan plot
#'
#' A fan plot consist of a set of transparent ribbons each representing a
#' different coverage of the uncertainty around an estimate. The coverages are
#' based on the assumption of a normal distribution with mean `link(y)` and
#' standard error `link_sd`.
#' @inheritParams ggplot2::stat_bin
#' @param fine a logical value. `TRUE` displays coverages from 10\% to 90\% in
#' steps of 10\%. `FALSE` displays coverages from 30\% to 90\% in steps of 30\%.
#' @param link the link function to apply on the `y` before calculting the
#' coverage intervals. Note that `link_sd` is the standard error on the link
#' scale, while `y` is on the natural scale. Defaults to `'identify'` which
#' implies no transformation. Other options are `'log'` and `'logit'`.
#' @export
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom ggplot2 layer
#' @family geom
#' @examples
#' z <- data.frame(year = 1990:2019, dx = rnorm(30), s = rnorm(30, 1, 0.05))
#' z$index <- cumsum(z$dx)
#' library(ggplot2)
#' ggplot(z, aes(x = year, y = index, link_sd = s)) + stat_fan()
#' ggplot(z, aes(x = year, y = index, link_sd = s)) + stat_fan() + geom_line()
#' ggplot(z, aes(x = year, y = index, link_sd = s)) + stat_fan(fine = TRUE)
#' ggplot(z, aes(x = year, y = exp(index), link_sd = s)) +
#'   stat_fan(link = "log") + geom_line()
#' ggplot(z, aes(x = year, y = plogis(index), link_sd = s)) +
#'   stat_fan(link = "logit") + geom_line()
#'
#' z <- expand.grid(year = 1990:2019, category = c("A", "B"))
#' z$dx <- rnorm(60, sd = 0.1)
#' z$index <- rep(c(0, 2), each = 30) + cumsum(z$dx)
#' z$s <- rnorm(60, rep(c(0.5, 1), each = 30), 0.05)
#' ggplot(z, aes(x = year, y = index, link_sd = s)) + stat_fan() + geom_line() +
#'   facet_wrap(~category)
#' ggplot(z, aes(x = year, y = index, link_sd = s)) +
#'   stat_fan(aes(fill = category)) + geom_line(aes(colour = category))
stat_fan <- function(
  mapping = NULL, data = NULL, position = "identity", na.rm = FALSE, # nolint
  show.legend = NA, inherit.aes = TRUE, ..., fine = FALSE, # nolint
  link = c("identity", "log", "logit")) {
  assert_that(is.flag(fine), noNA(fine))
  link <- match.arg(link)
  if (fine) {
    coverage <- seq(0.9, 1e-3, by = -0.1)
  } else {
    coverage <- seq(0.9, 1e-3, by = -0.3)
  }
  alpha <- 0.9 / length(coverage)
  lapply(
    coverage,
    function(i) {
      layer(
        stat = StatFan, data = data, mapping = mapping, geom = "ribbon",
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(coverage = i, link = link, na.rm = na.rm, ...,
                      alpha = alpha)
      )
    }
  )
}

#' @rdname stat_fan
#' @format NULL
#' @usage NULL
#' @importFrom assertthat assert_that
#' @importFrom ggplot2 ggproto Stat
#' @export
StatFan <- ggproto(
  "StatFan", Stat,
  compute_group = function(
    data, scales, coverage = 0.9, link = "identity"
  ) {
    switch(
      link,
      "identity" = {
        back <- trans <- function(x) {
          x
        }
      },
      "log" = {
        trans <- log
        back <- exp
      },
      "logit" = {
        trans <- qlogis
        back <- plogis
      }
    )
    data$ymin <- back(
      qnorm(p = 0.5 - coverage / 2, mean = trans(data$y), sd = data$link_sd)
    )
    data$ymax <- back(
      qnorm(p = 0.5 + coverage / 2, mean = trans(data$y), sd = data$link_sd)
    )
    return(data)
  },
  required_aes = c("x", "y", "link_sd")
)

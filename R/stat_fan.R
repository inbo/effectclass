#' Display a fan plot
#'
#' A fan plot consist of a set of transparent ribbons each representing a
#' different coverage of the uncertainty around an estimate.
#' @inheritParams ggplot2::stat_bin
#' @param fine a logical value. `TRUE` displays coverages from 10\% to 90\% in
#' steps of 10\%. `FALSE` displays coverages from 30\% to 90\% in steps of 30\%.
#' @export
#' @importFrom ggplot2 layer
#' @examples
#' z <- data.frame(year = 1990:2019, dx = rnorm(30), s = rnorm(30, 1, 0.05))
#' z$index <- cumsum(z$dx)
#' library(ggplot2)
#' ggplot(z, aes(x = year, y = index, sd = s)) + stat_fan()
#' ggplot(z, aes(x = year, y = index, sd = s)) + stat_fan() + geom_line()
#' ggplot(z, aes(x = year, y = index, sd = s)) + stat_fan(fine = TRUE)
#'
#' z <- expand.grid(year = 1990:2019, category = c("A", "B"))
#' z$dx <- rnorm(60, sd = 0.1)
#' z$index <- rep(c(0, 2), each = 30) + cumsum(z$dx)
#' z$s <- rnorm(60, rep(c(0.5, 1), each = 30), 0.05)
#' ggplot(z, aes(x = year, y = index, sd = s)) + stat_fan() + geom_line() +
#'   facet_wrap(~category)
#' ggplot(z, aes(x = year, y = index, sd = s)) +
#'   stat_fan(aes(fill = category)) + geom_line(aes(colour = category))
stat_fan <- function(mapping = NULL, data = NULL,
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, ..., fine = FALSE) {
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
        params = list(coverage = i, na.rm = na.rm, ..., alpha = alpha)
      )
    }
  )
}

#' @importFrom ggplot2 ggproto Stat
StatFan <- ggproto(
  "StatFan", Stat,
  compute_group = function(data, scales, coverage) {
    data$ymin <- qnorm(p = 0.5 - coverage / 2, mean = data$y, sd = data$sd)
    data$ymax <- qnorm(p = 0.5 + coverage / 2, mean = data$y, sd = data$sd)
    return(data)
  },
  required_aes = c("x", "y", "sd")
)

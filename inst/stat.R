library(ggplot2)
library(tidyverse)
n_year <- 30
n_month <- 4
expand.grid(
  year = seq_len(n_year),
  month = factor(seq_len(n_month))
) %>%
  mutate(
    dx = rnorm(n(), sd = 0.1),
    s = rnorm(n(), 0.1, 0.01)
  ) %>%
  group_by(month) %>%
  mutate(index = cumsum(dx)) %>%
  ggplot(aes(x = year, y = index, sd = s)) +
  stat_fan(aes(fill = month), fine = TRUE) +
  geom_line(aes(colour = month))
StatFan60 <- ggproto(
  "StatFan60", Stat,
  compute_group = function(data, scales) {
    data$ymin <- qnorm(p = 0.5 - 0.6 / 2, mean = data$y, sd = data$sd)
    data$ymax <- qnorm(p = 0.5 + 0.6 / 2, mean = data$y, sd = data$sd)
    return(data)
  },
  required_aes = c("x", "y", "sd")
)
StatFan30 <- ggproto(
  "StatFan30", Stat,
  compute_group = function(data, scales) {
    data$ymin <- qnorm(p = 0.5 - 0.3 / 2, mean = data$y, sd = data$sd)
    data$ymax <- qnorm(p = 0.5 + 0.3 / 2, mean = data$y, sd = data$sd)
    return(data)
  },
  required_aes = c("x", "y", "sd")
)
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
ggplot(x, aes(x = year, y = index, sd = s)) + stat_fan() + geom_line()
ggplot(x, aes(x = year, y = index, sd = s)) + stat_fan(fine = TRUE) + geom_line()

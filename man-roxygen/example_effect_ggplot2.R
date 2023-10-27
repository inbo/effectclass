#' @examples
#' oldw <- getOption("warn")
#' options(warn = -1)
#' library(ggplot2)
#' theme_set(theme_grey(base_family = "Helvetica"))
#' update_geom_defaults("point", list(size = 5))
#' ggplot(z, aes(x = effect, y = estimate, ymin = lcl, ymax = ucl)) +
#'   stat_effect(threshold = 1) +
#'   coord_flip()
#' ggplot(z[3:5, ], aes(x = effect, y = estimate, ymin = lcl, ymax = ucl)) +
#'   stat_effect(threshold = 1, ref_line = "none") +
#'   coord_flip()
#' ggplot(z[3:5, ], aes(x = effect, y = estimate, ymin = lcl, ymax = ucl)) +
#'   stat_effect(threshold = 1, errorbar = FALSE) +
#'   coord_flip()
#'
#' # plot indices
#' ggplot(trend, aes(x = year, y = index, ymin = lcl, ymax = ucl, sd = sd)) +
#'   geom_line() +
#'   stat_effect(threshold = th, reference = ref)
#'
#' # plot pairwise differences
#' change_set <- function(z, base_year) {
#'   n_year <- max(z$dt)
#'   total_change <- lapply(
#'     seq_len(n_year) - 1,
#'     function(i) {
#'       if (i > 0) {
#'         y <- tail(z, -i)
#'       } else {
#'         y <- z
#'       }
#'       data.frame(
#'         from = base_year + i, to = base_year + y$dt,
#'         total = cumsum(y$change), sd = sqrt(cumsum(y$sd ^ 2))
#'       )
#'     }
#'   )
#'   total_change <- do.call(rbind, total_change)
#'   total_change <- rbind(
#'     total_change,
#'     data.frame(
#'       from = total_change$to, to = total_change$from,
#'       total = -total_change$total, sd = total_change$sd
#'     )
#'   )
#'   total_change$lcl <- qnorm(0.025, total_change$total, total_change$sd)
#'   total_change$ucl <- qnorm(0.975, total_change$total, total_change$sd)
#'   return(total_change)
#' }
#' head(trend, 10) |>
#'   change_set(base_year) |>
#'   ggplot(aes(x = from, y = to, ymin = lcl, ymax = ucl)) +
#'   stat_effect(
#'     threshold = th, reference = ref, aes(colour = total), ref_line = "none",
#'     errorbar = FALSE, shape_colour = FALSE
#'   ) +
#'   scale_colour_gradient2()
#' head(trend, 10) |>
#'   change_set(base_year) |>
#'   ggplot(aes(x = from, y = to, ymin = lcl, ymax = ucl)) +
#'   stat_effect(
#'     threshold = th, reference = ref, ref_line = "none", errorbar = FALSE
#'   )
#' options(warn = oldw)

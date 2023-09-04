#' @examples
#' # All possible classes
#' z <- data.frame(
#'   estimate = c(-0.5, 0, 0.5, 1.5, 1, 0.5, 0, -0.5, -1, -1.5),
#'   sd = c(rep(0.8, 3), rep(0.3, 7))
#' )
#' z$lcl <- qnorm(0.05, z$estimate, z$sd)
#' z$ucl <- qnorm(0.95, z$estimate, z$sd)
#' classification(z$lcl, z$ucl, threshold = 1) -> z$effect
#' c(
#'   "?" = "unknown\neffect", "?+" = "potential\npositive\neffect",
#'   "?-" = "potential\nnegative\neffect", "~" = "no effect",
#'   "+" = "positive\neffect", "-" = "negative\neffect",
#'   "+~" = "moderate\npositive\neffect", "-~" = "moderate\nnegative\neffect",
#'   "++" = "strong\npositive\neffect", "--" = "strong\nnegative\neffect"
#' )[as.character(z$effect)] -> z$x
#' z$x <- factor(z$x, z$x)
#' z$display <- format_ci(z$estimate, lcl = z$lcl, ucl = z$ucl)
#'
#' # Simulated trend
#' set.seed(20190521)
#' base_year <- 2000
#' n_year <- 20
#' trend <- data.frame(
#'   dt = seq_len(n_year),
#'   change = rnorm(n_year, sd = 0.2),
#'   sd = rnorm(n_year, mean = 0.1, sd = 0.01)
#' )
#' trend$index <- cumsum(trend$change)
#' trend$lcl <- qnorm(0.025, trend$index, trend$sd)
#' trend$ucl <- qnorm(0.975, trend$index, trend$sd)
#' trend$year <- base_year + trend$dt
#' trend$display <- format_ci(trend$index, lcl = trend$lcl, ucl = trend$ucl)
#' th <- 0.25
#' ref <- 0

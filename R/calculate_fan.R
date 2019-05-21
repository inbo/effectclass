#' calculate confidence intervals for different coverages
#'
#' @param x a `data.frame`
#' @param fit the variable from `x` that contains the fitted value
#' @param fit_se the variable from `x` that contains the standard error of the
#' fitted value
#' @param coverage a vector with coverage values
#' @return the original data.frame with extra variables `lcl`, `ucl` and
#' `coverage`.
#' @export
#' @importFrom assertthat assert_that is.string has_name noNA
#' @importFrom stats qnorm
calculate_fan <- function(x, fit, fit_se, coverage = c(0.3, 0.6, 0.9)) {
  assert_that(inherits(x, "data.frame"), is.string(fit), is.string(fit_se),
              has_name(x, c(fit, fit_se)), is.numeric(coverage),
              all(coverage > 0), all(coverage < 1), noNA(coverage))
  if (has_name(x, c("lcl", "ucl", "coverage"))) {
warning("the function overwrites existing variables 'lcl', 'ucl' or 'coverage'.")
  }
  add_ribbon <- function(x, fit, fit_se, coverage) {
    x$lcl <- qnorm(0.5 - coverage / 2, mean = x[[fit]], sd = x[[fit_se]])
    x$ucl <- qnorm(0.5 + coverage / 2, mean = x[[fit]], sd = x[[fit_se]])
    x$coverage <- coverage
    return(list(x))
  }
  x <- vapply(X = coverage, FUN = add_ribbon, FUN.VALUE = list(NULL),
              fit = fit, fit_se = fit_se, x = x)
  do.call(rbind, x)
}
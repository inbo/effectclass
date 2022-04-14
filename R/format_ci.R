#' Format an Estimate and Confidence Interval as Text
#'
#' The function rounds the estimate, lower and upper confidence interval to the
#' same magnitude.
#' The magnitude shows the width of the confidence interval with two significant
#' digits.
#' @param estimate The estimate in the `link` scale.
#' @param se The standard error in the `link` scale.
#' If missing, you must provide values for `lcl` and `ucl`.
#' @param lcl The lower confidence limit.
#' Ignored when `se` is given.
#' @param ucl The upper confidence limit.
#' Ignored when `se` is given.
#' @param interval The coverage of the confidence interval.
#' Only used when `se` is given.
#' Defaults to `0.95` (`95%`).
#' @param link The transformation of `estimate`, `se`, `lcl` and `ucl`.
#' The appropriate back transformation is applied before formatting.
#' @param max_digit The maximum number of significant digits to display.
#' Defaults to `4`.
#' @param percent Display the interval as a percentage
#' (= multiply by 100 and append `%`).
#' Defaults to `FALSE`.
#' @param sign Always add the sign to the text. (e.g. `+1` instead of `1`).
#' Defaults to `FALSE`.
#' @export
#' @importFrom assertthat assert_that is.flag is.number noNA
#' @importFrom stats plogis qnorm
#' @family display
#' @examples
#' format_ci(0.512345, 1)
#' format_ci(0.512345, 1, interval = 0.9)
#' format_ci(0.512345, 1, link = "log")
#' format_ci(0.512345, 1, link = "logit")
#' format_ci(0.512345, 10)
#' format_ci(0.512345, 0.1)
#' format_ci(0.512345, 0.01)
#' format_ci(0.512345, 0.001)
#' format_ci(0.512345, 0.0001)
#' format_ci(0.512345, 0.00001)
#' format_ci(0.512345, 0.00001, max_digit = 10)
#' format_ci(0.512345, 0.5)
#' format_ci(-0.1, lcl = -0.1999, ucl = 0.1234)
#' format_ci(-0.1, lcl = -0.1999, ucl = 0.1234, percent = TRUE)
#' format_ci(-0.1, lcl = -0.1999, ucl = 0.1234, sign = TRUE)
#' format_ci(-0.1, lcl = -0.1999, ucl = 0.1234, percent = TRUE, sign = TRUE)
#' format_ci(0.512345e-6, 1e-6)
#' format_ci(0.512345e-7, 1e-7)
#' format_ci(0.512345e-7, 1e-8)
#' format_ci(0.512345e-7, 1e-9)
format_ci <- function(
  estimate, se, lcl, ucl, interval = 0.95, link = c("identity", "log", "logit"),
  max_digit = 4, percent = FALSE, sign = FALSE
) {
  link <- match.arg(link)
  assert_that(
    is.numeric(estimate),
    noNA(estimate),
    is.number(max_digit),
    is.flag(percent),
    noNA(percent),
    is.flag(sign),
    noNA(sign)
  )
  if (missing(se)) {
    assert_that(
      is.numeric(lcl),
      is.numeric(ucl),
      noNA(lcl),
      noNA(ucl),
      all(lcl <= estimate),
      all(estimate <= ucl)
    )
  } else {
    assert_that(
      missing(lcl),
      missing(ucl),
      is.numeric(se),
      noNA(se),
      all(0 <= se),
      is.number(interval),
      0 < interval,
      interval < 1
    )
    lcl <- qnorm((1 - interval) / 2, mean = estimate, sd = se)
    ucl <- qnorm((1 + interval) / 2, mean = estimate, sd = se)
  }

  switch(
    link,
    log = {
      estimate <- exp(estimate)
      lcl <- exp(lcl)
      ucl <- exp(ucl)
    },
    logit = {
      estimate <- plogis(estimate)
      lcl <- plogis(lcl)
      ucl <- plogis(ucl)
    }
  )
  if (percent) {
    estimate <- 100 * estimate
    lcl <- 100 * lcl
    ucl <- 100 * ucl
  }

  ci_magnitude <- floor(log10(ucl - lcl)) - 2
  ci_range <- floor(log10(pmax(abs(ucl), abs(lcl))))
  check_one <- pmax(abs(ucl), abs(lcl)) / 10 ^ ci_range < 2
  signif_digit <- pmin(max_digit, ci_range - ci_magnitude) + check_one
  magnitude <- ci_range - signif_digit + 1

  ci_range <- ifelse(
    check_one,
    ci_range - max_digit,
    ci_range - max_digit - 2
  )
  magnitude <- pmax(magnitude, ci_range)
  fmt <- ifelse(
    magnitude >= -7,
    sprintf("%%.%if", pmax(0, -magnitude)),
    sprintf("%%.%ig", signif_digit)
  )
  if (sign) {
    fmt <- gsub("%", "%+", fmt)
  }
  if (percent) {
    fmt <- paste0(fmt, "%%")
  }
  sprintf(
    sprintf("%1$s (%1$s; %1$s)", fmt),
    round(estimate / 10 ^ magnitude) * 10 ^ magnitude,
    round(lcl / 10 ^ magnitude) * 10 ^ magnitude,
    round(ucl / 10 ^ magnitude) * 10 ^ magnitude
  )
}

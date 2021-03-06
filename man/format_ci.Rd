% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_ci.R
\name{format_ci}
\alias{format_ci}
\title{Format an Estimate and Confidence Interval as Text}
\usage{
format_ci(estimate, se, lcl, ucl, interval = 0.95, link = c("identity",
  "log", "logit"), max_digit = 4, percent = FALSE, sign = FALSE)
}
\arguments{
\item{estimate}{The estimate in the \code{link} scale.}

\item{se}{The standard error in the \code{link} scale.
If missing, you must provide values for \code{lcl} and \code{ucl}.}

\item{lcl}{The lower confidence limit.
Ignored when \code{se} is given.}

\item{ucl}{The upper confidence limit.
Ignored when \code{se} is given.}

\item{interval}{The coverage of the confidence interval.
Only used when \code{se} is given.
Defaults to \code{0.95} (\code{95\%}).}

\item{link}{The transformation of \code{estimate}, \code{se}, \code{lcl} and \code{ucl}.
The appropriate backtransformation is applied before formatting.}

\item{max_digit}{The maximum number of significant digits to display.
Defaults to \code{4}.}

\item{percent}{Display the interval as a percentage
(= muliply by 100 and append \code{\%}).
Defaults to \code{FALSE}.}

\item{sign}{Always add the sign to the text. (e.g. \code{+1} instead of \code{1}).
Defaults to \code{FALSE}.}
}
\description{
The function rounds the estimate, lower and upper confidence interval to the
same magnitude.
The magnitude shows the width of the confidence interval with two significant
digits.
}
\examples{
format_ci(0.512345, 1)
format_ci(0.512345, 1, interval = 0.9)
format_ci(0.512345, 1, link = "log")
format_ci(0.512345, 1, link = "logit")
format_ci(0.512345, 10)
format_ci(0.512345, 0.1)
format_ci(0.512345, 0.01)
format_ci(0.512345, 0.001)
format_ci(0.512345, 0.0001)
format_ci(0.512345, 0.00001)
format_ci(0.512345, 0.00001, max_digit = 10)
format_ci(0.512345, 0.5)
format_ci(-0.1, lcl = -0.1999, ucl = 0.1234)
format_ci(-0.1, lcl = -0.1999, ucl = 0.1234, percent = TRUE)
format_ci(-0.1, lcl = -0.1999, ucl = 0.1234, sign = TRUE)
format_ci(-0.1, lcl = -0.1999, ucl = 0.1234, percent = TRUE, sign = TRUE)
format_ci(0.512345e-6, 1e-6)
format_ci(0.512345e-7, 1e-7)
format_ci(0.512345e-7, 1e-8)
format_ci(0.512345e-7, 1e-9)
}
\concept{display}

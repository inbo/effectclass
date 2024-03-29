% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_fan.R
\name{add_fan}
\alias{add_fan}
\title{Add a fan plot to a \code{plotly} object}
\usage{
add_fan(
  p,
  x = NULL,
  y = NULL,
  ...,
  sd,
  link = c("identity", "log", "logit"),
  max_prob = 0.9,
  step = 0.05,
  fillcolor = coarse_unsigned_palette[2],
  data = NULL,
  inherit = TRUE,
  text = NULL,
  hoverinfo = "text",
  name
)
}
\arguments{
\item{p}{a plotly object}

\item{x}{the x variable.}

\item{y}{the variable median on the natural scale.}

\item{...}{Arguments (i.e., attributes) passed along to the trace \code{type}.
See \code{\link[plotly:schema]{schema()}} for a list of acceptable attributes for a given trace \code{type}
(by going to \code{traces} -> \code{type} -> \code{attributes}). Note that attributes
provided at this level may override other arguments
(e.g. \code{plot_ly(x = 1:10, y = 1:10, color = I("red"), marker = list(color = "blue"))}).}

\item{sd}{the variable of the standard error on the link scale.}

\item{link}{the link between the natural scale and the link scale.
Defaults to \code{"identity"}.}

\item{max_prob}{The coverage of the widest band.
Defaults to \code{0.9}.}

\item{step}{the step size between consecutive bands.
The function adds all bands with coverage \code{max_prob - i * step} for all
positive integer values \code{i} resulting in a positive coverage.
Defaults to \code{0.05}.}

\item{fillcolor}{The fill colour of the fan.
Defaults to a greyish blue.}

\item{data}{A data frame (optional) or \link[crosstalk:SharedData]{crosstalk::SharedData} object.}

\item{inherit}{inherit attributes from \code{\link[plotly:plot_ly]{plot_ly()}}?}

\item{text}{textual labels.}

\item{hoverinfo}{Which hover information to display.
Defaults to \code{"text"}.
When no \code{"text"} variable is specified, the function displays a formatted
confidence interval.}

\item{name}{Optional name of the trace for the legend.}
}
\description{
A fan plot consist of a set of transparent ribbons each representing a
different coverage of the uncertainty around an estimate.
The coverages are based on the assumption of a normal distribution with mean
\code{link(y)} and standard error \code{sd}.
}
\examples{
# All possible classes
z <- data.frame(
  estimate = c(-0.5, 0, 0.5, 1.5, 1, 0.5, 0, -0.5, -1, -1.5),
  sd = c(rep(0.8, 3), rep(0.3, 7))
)
z$lcl <- qnorm(0.05, z$estimate, z$sd)
z$ucl <- qnorm(0.95, z$estimate, z$sd)
classification(z$lcl, z$ucl, threshold = 1) -> z$effect
c(
  "?" = "unknown\neffect", "?+" = "potential\npositive\neffect",
  "?-" = "potential\nnegative\neffect", "~" = "no effect",
  "+" = "positive\neffect", "-" = "negative\neffect",
  "+~" = "moderate\npositive\neffect", "-~" = "moderate\nnegative\neffect",
  "++" = "strong\npositive\neffect", "--" = "strong\nnegative\neffect"
)[as.character(z$effect)] -> z$x
z$x <- factor(z$x, z$x)
z$display <- paste(
  "estimate:", format_ci(z$estimate, lcl = z$lcl, ucl = z$ucl)
)

# Simulated trend
set.seed(20190521)
base_year <- 2000
n_year <- 20
trend <- data.frame(
  dt = seq_len(n_year),
  change = rnorm(n_year, sd = 0.2),
  sd = rnorm(n_year, mean = 0.1, sd = 0.01)
)
trend$index <- cumsum(trend$change)
trend$lcl <- qnorm(0.025, trend$index, trend$sd)
trend$ucl <- qnorm(0.975, trend$index, trend$sd)
trend$year <- base_year + trend$dt
trend$display <- paste(
  "index:", format_ci(trend$index, lcl = trend$lcl, ucl = trend$ucl)
)
th <- 0.25
ref <- 0
library(plotly)
plot_ly(z, x = ~x, y = ~estimate) |>
  add_fan(sd = ~sd, text = ~display) |>
  add_classification(lcl = ~lcl, ucl = ~ucl, threshold = 1) |>
  layout(
    hovermode = "x unified",
    shapes = reference_shape(threshold = 1),
    annotations = reference_text(threshold = 1)
  )
plot_ly(z, x = ~x, y = ~estimate) |>
  add_fan(sd = ~sd, step = 0.1, text = ~display) |>
  add_classification(
    lcl = ~lcl, ucl = ~ucl, threshold = 1, detailed = FALSE
   ) |>
  layout(
    shapes = reference_shape(threshold = 1, line = TRUE),
    annotations = reference_text(threshold = 1)
   )
plot_ly(z, x = ~x, y = ~estimate) |>
  add_fan(sd = ~sd, step = 0.2, hoverinfo = "none") |>
  add_classification(
    lcl = ~lcl, ucl = ~ucl, threshold = 1, signed = FALSE
  ) |>
  layout(shapes = reference_shape(threshold = 1))
plot_ly(z, x = ~x, y = ~estimate) |>
  add_fan(sd = ~sd, step = 0.3) |>
  add_classification(
    lcl = ~lcl, ucl = ~ucl, threshold = 1, detailed = FALSE, signed = FALSE,
    text = ~display
  ) |>
  layout(
    shapes = reference_shape(threshold = 1, line = TRUE)
  )

# trend
plot_ly(data = trend, x = ~year, y = ~index) |>
  add_fan(sd = ~sd, text = ~display, hoverinfo = "text") |>
  add_classification(sd = ~sd, threshold = th) |>
  layout(
    hovermode = "x unified", hoverdistance = 1,
    shapes = reference_shape(threshold = th, reference = ref),
    annotations = reference_text(threshold = th, reference = ref)
  )
}
\seealso{
Other plotly add-ons: 
\code{\link{add_classification}()},
\code{\link{reference_shape}()},
\code{\link{reference_text}()}
}
\concept{plotly add-ons}

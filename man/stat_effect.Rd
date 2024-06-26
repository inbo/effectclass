% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_effect.R
\name{stat_effect}
\alias{stat_effect}
\title{Display points with classified effect}
\usage{
stat_effect(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  threshold,
  reference = 0,
  detailed = TRUE,
  signed = TRUE,
  shape_colour = TRUE,
  errorbar = TRUE,
  error_colour = TRUE,
  size = 6,
  labels = class_labels(lang = "en", detailed = detailed, signed = signed),
  ref_line = c("all", "ref", "none")
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[ggplot2:layer_positions]{layer position} documentation.
}}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}'s \code{params} argument. These
arguments broadly fall into one of 4 categories below. Notably, further
arguments to the \code{position} argument, or aesthetics that are required
can \emph{not} be passed through \code{...}. Unknown arguments that are not part
of the 4 categories below are ignored.
\itemize{
\item Static aesthetics that are not mapped to a scale, but are at a fixed
value and apply to the layer as a whole. For example, \code{colour = "red"}
or \code{linewidth = 3}. The geom's documentation has an \strong{Aesthetics}
section that lists the available options. The 'required' aesthetics
cannot be passed on to the \code{params}. Please note that while passing
unmapped aesthetics as vectors is technically possible, the order and
required length is not guaranteed to be parallel to the input data.
\item When constructing a layer using
a \verb{stat_*()} function, the \code{...} argument can be used to pass on
parameters to the \code{geom} part of the layer. An example of this is
\code{stat_density(geom = "area", outline.type = "both")}. The geom's
documentation lists which parameters it can accept.
\item Inversely, when constructing a layer using a
\verb{geom_*()} function, the \code{...} argument can be used to pass on parameters
to the \code{stat} part of the layer. An example of this is
\code{geom_area(stat = "density", adjust = 0.5)}. The stat's documentation
lists which parameters it can accept.
\item The \code{key_glyph} argument of \code{\link[ggplot2:layer]{layer()}} may also be passed on through
\code{...}. This can be one of the functions described as
\link[ggplot2:draw_key]{key glyphs}, to change the display of the layer in the legend.
}}

\item{threshold}{A vector of either 1 or 2 thresholds.
A single threshold will be transformed into
\code{reference + c(-abs(threshold), abs(threshold))}.}

\item{reference}{The null hypothesis.
Defaults to 0.}

\item{detailed}{\code{TRUE} indicates a detailed \code{\link[=classification]{classification()}};
\code{FALSE} a \code{\link[=coarse_classification]{coarse_classification()}}.
Defaults to \code{TRUE}.}

\item{signed}{\code{TRUE} indicates a signed classification;
\code{FALSE} a classification with \code{\link[=remove_sign]{remove_sign()}}.
Defaults to \code{TRUE}.}

\item{shape_colour}{Colour the background of the labels according to the
classification.
Defaults to \code{TRUE}.}

\item{errorbar}{Display the uncertainty as error bars.
Defaults to \code{TRUE}.}

\item{error_colour}{Colour the error bars according to the classification.
Defaults to \code{TRUE}.}

\item{size}{Size of the symbols.}

\item{labels}{the labels for the legend.}

\item{ref_line}{Which reference lines to display.
\code{"all"} displays a dashed horizontal line at the \code{reference} and a dotted
horizontal line at the \code{threshold}.
\code{"ref"} displays a dashed horizontal line at the \code{reference}.
\code{"none"} displays no horizontal lines.}
}
\description{
Display points with classified effect
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
oldw <- getOption("warn")
options(warn = -1)
library(ggplot2)
theme_set(theme_grey(base_family = "Helvetica"))
update_geom_defaults("point", list(size = 5))
ggplot(z, aes(x = effect, y = estimate, ymin = lcl, ymax = ucl)) +
  stat_effect(threshold = 1) +
  coord_flip()
ggplot(z[3:5, ], aes(x = effect, y = estimate, ymin = lcl, ymax = ucl)) +
  stat_effect(threshold = 1, ref_line = "none") +
  coord_flip()
ggplot(z[3:5, ], aes(x = effect, y = estimate, ymin = lcl, ymax = ucl)) +
  stat_effect(threshold = 1, errorbar = FALSE) +
  coord_flip()

# plot indices
ggplot(trend, aes(x = year, y = index, ymin = lcl, ymax = ucl, sd = sd)) +
  geom_line() +
  stat_effect(threshold = th, reference = ref)

# plot pairwise differences
change_set <- function(z, base_year) {
  n_year <- max(z$dt)
  total_change <- lapply(
    seq_len(n_year) - 1,
    function(i) {
      if (i > 0) {
        y <- tail(z, -i)
      } else {
        y <- z
      }
      data.frame(
        from = base_year + i, to = base_year + y$dt,
        total = cumsum(y$change), sd = sqrt(cumsum(y$sd ^ 2))
      )
    }
  )
  total_change <- do.call(rbind, total_change)
  total_change <- rbind(
    total_change,
    data.frame(
      from = total_change$to, to = total_change$from,
      total = -total_change$total, sd = total_change$sd
    )
  )
  total_change$lcl <- qnorm(0.025, total_change$total, total_change$sd)
  total_change$ucl <- qnorm(0.975, total_change$total, total_change$sd)
  return(total_change)
}
head(trend, 10) |>
  change_set(base_year) |>
  ggplot(aes(x = from, y = to, ymin = lcl, ymax = ucl)) +
  stat_effect(
    threshold = th, reference = ref, aes(colour = total), ref_line = "none",
    errorbar = FALSE, shape_colour = FALSE
  ) +
  scale_colour_gradient2()
head(trend, 10) |>
  change_set(base_year) |>
  ggplot(aes(x = from, y = to, ymin = lcl, ymax = ucl)) +
  stat_effect(
    threshold = th, reference = ref, ref_line = "none", errorbar = FALSE
  )
options(warn = oldw)
}
\seealso{
\code{\link{classification}}

Other ggplot2 add-ons: 
\code{\link{scale_effect}()},
\code{\link{stat_fan}()}
}
\concept{ggplot2 add-ons}

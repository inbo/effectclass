% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coarse_classification.R
\name{coarse_classification}
\alias{coarse_classification}
\title{Use a Lower Scale Classification}
\usage{
coarse_classification(classification)
}
\arguments{
\item{classification}{The classification}
}
\description{
\code{coarse_classification(y)} reduces the 10 scales from
\code{y <- classification(x)} to the 4 scales below.
\itemize{
\item \code{+} \strong{positive effect}: \code{reference < lcl}
\item \code{~} \strong{no effect}: \verb{min(threshold) < lcl < reference} and
\verb{reference < ucl < max(threshold)}
\item \code{-} \strong{negative effect}: \code{ucl < reference}
\item \verb{?} \strong{unknown effect}: \code{lcl < min(threshold)} or \code{max(threshold) < ucl}
}

\code{coarse_classification(y)} reduces the 6 scales from
\code{y <- remove_sign(classification(x))} into 3 scales.
}
\seealso{
Other classification functions: 
\code{\link{classification}()},
\code{\link{remove_sign}()}
}
\concept{classification functions}

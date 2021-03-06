% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derivative.R
\name{pev_data_derivative}
\alias{pev_data_derivative}
\title{Get perceptual-derivative of continuous-palette}
\usage{
pev_data_derivative(.fcont, n_panel = 40, method = "cie2000",
  include_cvd = TRUE)
}
\arguments{
\item{.fcont}{\code{object} that can be coerced to \code{pev_fcont},
when called with a numeric vector with values
between 0 and 1, returns the corresponding (hex-code) values.}

\item{n_panel}{\code{numeric}, number of panels into which to discretize the
palette-function.}

\item{method}{\code{character}, describes how the domain of the continuous palette
is to be discretized, can be \code{"post"} or \code{"panel"} (using a fencing analogy).}

\item{include_cvd}{\code{logical} or \code{character}, indicates to include data for
for color-vision deficiency. Possible character values: \code{"none"}, \code{"protan"},
\code{"deutan"}, \code{"tritan"}.}
}
\value{
\code{data.frame} with variables \code{cvd}, \code{x}, \code{hex}, \code{d_distance_d_x}
}
\description{
Get perceptual-derivative of continuous-palette
}
\examples{
  pev_data_derivative("Viridis")
}

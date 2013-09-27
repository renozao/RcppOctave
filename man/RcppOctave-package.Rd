\docType{package}
\name{RcppOctave}
\alias{RcppOctave}
\alias{RcppOctave-package}
\title{Interfacing R with Octave}
\description{
  Interfacing R with Octave.
}
\details{
  The primary goal is to facilitate the port of
  Matlab/Octave scripts to R. The package enables to call
  any Octave functions from R and as well as browsing their
  documentation, passing variables between R and Octave,
  using R core RNGs in Octave, which ensure stochastic
  computations are also reproducible.

  \tabular{ll}{ Package: \tab RcppOctave\cr Type: \tab
  Package\cr Version: \tab 1.0\cr Date: \tab 2011-11-01\cr
  License: \tab GPL (>= 2)\cr LazyLoad: \tab yes\cr }
}
\examples{
.CallOctave('svd', matrix(1:9, 3))
o_help('svd')
}
\author{
  Renaud Gaujoux \email{renaud@cbio.uct.ac.za}

  Maintainer: Renaud Gaujoux \email{renaud@cbio.uct.ac.za}
}
\references{
  Eaton JW (2002). _GNU Octave Manual_. Network Theory
  Limited. ISBN 0-9541617-2-6, <URL:
  http://www.octave.org/>.
}
\seealso{
  See \code{\link{.CallOctave}}, \code{\link{o_source}},
  \code{\link{o_help}}
}
\keyword{package}


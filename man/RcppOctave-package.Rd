\docType{package}
\name{RcppOctave}
\alias{RcppOctave}
\alias{RcppOctave-package}
\title{Seamless Interface to Octave -- and Matlab}
\description{
  Direct interface to Octave. The primary goal is to
  facilitate the port of Matlab/Octave scripts to R. The
  package enables to call any Octave functions from R and
  as well as browsing their documentation, passing
  variables between R and Octave, using R core RNGs in
  Octave, which ensures that stochastic computations are
  also reproducible.

  The primary goal is to facilitate the port of
  Matlab/Octave scripts to R. The package enables to call
  any Octave functions from R and as well as browsing their
  documentation, passing variables between R and Octave,
  using R core RNGs in Octave, which ensure stochastic
  computations are also reproducible.
}
\details{
  \tabular{ll}{ Package: \tab RcppOctave\cr Type: \tab
  Package\cr Version: \tab 0.14.5\cr Date: \tab
  2013-11-07\cr License: \tab GPL (>=2)\cr LazyLoad: \tab
  yes\cr }

  \tabular{ll}{ Package: \tab RcppOctave\cr Type: \tab
  Package\cr Version: \tab 1.0\cr Date: \tab 2011-11-01\cr
  License: \tab GPL (>= 2)\cr LazyLoad: \tab yes\cr }
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

.CallOctave('svd', matrix(1:9, 3))
o_help('svd')
}
\author{
  Renaud Gaujoux

Maintainer: Renaud Gaujoux <renaud@tx.technion.ac.il>

Renaud Gaujoux \email{renaud@tx.technion.ac.il}

Maintainer: Renaud Gaujoux \email{renaud@tx.technion.ac.il}
}
\references{
  Eaton JW (2002). _GNU Octave Manual_. Network Theory
  Limited. ISBN 0-9541617-2-6, <URL:
  http://www.octave.org/>.
}
\seealso{
  See \code{\link{.CallOctave}}, \code{\link{o_source}}, \code{\link{o_help}}
}
\keyword{package}


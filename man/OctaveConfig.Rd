\name{OctaveConfig}
\alias{OctaveConfig}
\title{Octave-RcppOctave Configuration}
\usage{
  OctaveConfig(name, ..., path = NULL)
}
\arguments{
  \item{name}{Name of an RcppOctave path variable}

  \item{...}{extra names to be concatenated to the result
  with \code{\link{file.path}}. Only used when \code{name}
  is not missing.}

  \item{path}{path to Octave bin/ sub-directory}
}
\value{
  a list (if \code{name is missing}) or a single character
  string.
}
\description{
  Configures Octave and load RcppOctave
}
\examples{
OctaveConfig()
OctaveConfig('lib')
OctaveConfig('include')
OctaveConfig('modules')
}
\keyword{internal}


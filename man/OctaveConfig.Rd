\name{OctaveConfig}
\alias{OctaveConfig}
\title{Cached RcppOctave Configuration Paths}
\usage{
  OctaveConfig(name, ..., reset = FALSE, check = FALSE)
}
\arguments{
  \item{name}{Name of an RcppOctave path variable}

  \item{...}{extra names to be concatenated to the result
  with \code{\link{file.path}}. Only used when \code{name}
  is not missing.}

  \item{reset}{logical that indicates if the configuration
  should be reloaded}

  \item{check}{logical that indicates if the presence of a
  working version of the \code{octave-config} binary should
  be tested before trying to load the configuration.}
}
\value{
  a list (if \code{name is missing}) or a single character
  string.
}
\description{
  Cached RcppOctave Configuration Paths
}
\examples{
OctaveConfig()
OctaveConfig('lib')
OctaveConfig('include')
OctaveConfig('modules')
}
\keyword{internal}


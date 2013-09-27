\name{o_version}
\alias{o_version}
\title{Get Octave Version}
\usage{
  o_version(version)
}
\arguments{
  \item{version}{optional reference version to compare
  with.}
}
\value{
  Octave version as a single character string or the result
  of \code{\link[utils]{compareVersion}} if argument
  \code{version} is provided.
}
\description{
  Returns the version of Octave currently used by
  \code{RcppOctave}.
}
\examples{
o_version()
o_version("3.6.2")
o_version("3.4")
}


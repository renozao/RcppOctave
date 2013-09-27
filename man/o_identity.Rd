\name{o_identity}
\alias{o_identity}
\title{Octave Identity Function}
\usage{
  o_identity(...)
}
\arguments{
  \item{...}{any R object supported by RcppOctave.}
}
\value{
  its argument -- list -- after its conversion from R to
  Octave and from Octave to R.
}
\description{
  This function calls the Octave function provided by the
  module shipped with RcppOctave. It Returns its arguments
  unchanged, and is mainly used to test and check the
  effect of object conversions between R and Octave.
}
\examples{
o_identity(1L)
o_identity(1:10)
o_identity(matrix(1:10, 2,5))

o_identity(1)
o_identity(runif(10))
o_identity(matrix(runif(10), 2,5))
}


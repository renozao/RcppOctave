\name{o_rnorm}
\alias{o_rnorm}
\title{Drawing from R Normal Distribution in Octave}
\usage{
  o_rnorm(n, p = n)
}
\arguments{
  \item{n}{number of output rows}

  \item{p}{number of output columns (default to \code{n})}
}
\description{
  This function wraps a call to the standard Octave
  function \code{randn}, which is redefined by
  \code{RcppOctave} to call the R base function
  \code{\link[stats]{rnorm}}. This enables to exactly
  reproduce stochastic computations in R and Octave,
  without changing the original Octave/Matlab code. See
  \code{\link{o_runif}} for more details.
}
\section{Octave Documentation for \emph{randn}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(randn,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
# Draw random normal values (in vector form)
set.seed(123)
o_rnorm(1)
o_rnorm(1, 10)

# Draw random normal values (in matrix form)
set.seed(123)
o_rnorm(2)
o_rnorm(2, 5)
}
\seealso{
  rnorm

  Other orandom: \code{\link{o_rexp}},
  \code{\link{o_rgamma}}, \code{\link{o_runif}}
}


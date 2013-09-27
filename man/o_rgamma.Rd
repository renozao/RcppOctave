\name{o_rgamma}
\alias{o_rgamma}
\title{Drawing from R Gamma Distribution in Octave}
\usage{
  o_rgamma(n, p = n, shape = 1, scale = 1)
}
\arguments{
  \item{shape}{Mean of the Gamma distribution}

  \item{scale}{Scale of the Gamma distribution}

  \item{n}{number of output rows}

  \item{p}{number of output columns (default to \code{n})}
}
\description{
  This function wraps a call to the standard Octave
  function \code{randg}, which is redefined by
  \code{RcppOctave} to call the R base function
  \code{\link[stats]{rgamma}}. This enables to exactly
  reproduce stochastic computations in R and Octave,
  without changing the original Octave/Matlab code. See
  \code{\link{o_runif}} for more details.
}
\section{Octave Documentation for \emph{randg}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(randg,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
# Draw random gamma values (in vector form)
set.seed(123)
o_rgamma(1)
o_rgamma(1, 10)

# Draw random gamma values (in matrix form)
set.seed(123)
o_rgamma(2)
o_rgamma(2, 5)

# Draw random gamma values with shape and scale parameters
o_rgamma(1, 5, shape=2)
o_rgamma(1, 10, scale=0.5)
}
\seealso{
  rgamma

  Other orandom: \code{\link{o_rexp}},
  \code{\link{o_rnorm}}, \code{\link{o_runif}}
}


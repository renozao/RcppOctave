\name{o_rexp}
\alias{o_rexp}
\title{Drawing from R Exponential Distribution in Octave}
\usage{
  o_rexp(n, p = n)
}
\arguments{
  \item{n}{number of output rows}

  \item{p}{number of output columns (default to \code{n})}
}
\description{
  This function wraps a call to the standard Octave
  function \code{rande}, which is redefined by
  \code{RcppOctave} to call the R base function
  \code{\link[stats]{rexp}}. This enables to exactly
  reproduce stochastic computations in R and Octave,
  without changing the original Octave/Matlab code. See
  \code{\link{o_runif}} for more details.
}
\section{Octave Documentation for \emph{rande}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(rande,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
# Draw random exponential values (in vector form)
set.seed(123)
o_rexp(1)
o_rexp(1, 10)

# Draw random normal values (in matrix form)
set.seed(123)
o_rexp(2)
o_rexp(2, 5)
}
\seealso{
  rexp

  Other orandom: \code{\link{o_rgamma}},
  \code{\link{o_rnorm}}, \code{\link{o_runif}}
}


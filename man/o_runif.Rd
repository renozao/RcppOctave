\name{o_runif}
\alias{o_runif}
\title{Drawing from R Uniform Distribution in Octave}
\usage{
  o_runif(n, p = n)
}
\arguments{
  \item{n}{number of output rows}

  \item{p}{number of output columns (default to \code{n})}
}
\value{
  a numeric vector or a matrix
}
\description{
  This function wraps a call to the standard Octave
  function \code{rand}, which is redefined by
  \code{RcppOctave} to call the R base function
  \code{\link[stats]{runif}}. This enables to exactly
  reproduce stochastic computations in R and Octave,
  without changing the original Octave/Matlab code.
}
\section{Difference with plain \code{runif}}{
  Since calling \code{o_runif} or
  \code{\link[stats]{runif}} is equivalent, this function
  may not be really useful for the end user, and is defined
  for testing purposes essentially. One possible advantage
  over plain \code{\link[stats]{runif}} however, is that it
  can generate random matrices, instead of only vectors
  (see examples).
}

\section{Seeding}{
  Because the RNG of R is called used, seeding computations
  is achieved by a standard call to \code{\link{set.seed}}.
}

\section{Octave details}{
  \code{RcppOctave} defines a set of functions like
  \code{rand} that shadow Octave built-in functions. These
  functions are defined in the Octave module
  \code{Rrng.oct} that is stored in the package
  \emph{modules/} sub-directory. Call
  \code{OctaveConfig('modules')} to see this directory's
  full path.
}

\section{Octave Documentation for \emph{rand}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(rand,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
# Draw random uniform values (in vector form)
set.seed(123)
o_runif(1)
o_runif(1, 10)
# The result is identical as calling runif
set.seed(123)
runif(1)
runif(10)

# Draw random uniform values (in matrix form)
set.seed(123)
o_runif(2)
o_runif(2, 5)
}
\seealso{
  runif

  Other orandom: \code{\link{o_rexp}},
  \code{\link{o_rgamma}}, \code{\link{o_rnorm}}
}


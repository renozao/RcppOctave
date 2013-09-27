\name{o_load}
\alias{o_load}
\title{Loading Variables into Octave}
\usage{
  o_load(from, ..., options)
}
\arguments{
  \item{from}{a list or an environment from where the
  objects should be loaded}

  \item{...}{names of the variables to load}

  \item{options}{argument passed to the Octave function
  \code{load}. See section \emph{Octave Documentation}.}
}
\description{
  Loads variables from a file, a list or an environment.
}
\section{Octave Documentation for \emph{load}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(load,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
# Loading from a MATLAB/Octave file
#o_load

# Loading from an R list
o_clear()
l <- list(a=1, b=20, c=runif(10), d="this is a string", e=matrix(1:15, 3, 5))
o_load(l)

# Loading from an R environment
o_load( list2env(l) )

# Partial loading
o_clear()
o_load(l, a, b, c)
o_clear()
o_load(list2env(l), d, e)
}


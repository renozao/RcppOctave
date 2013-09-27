\name{o_exist}
\alias{o_exist}
\title{Checking Octave Variables}
\usage{
  o_exist(NAME, ...)
}
\arguments{
  \item{NAME}{name to check existence.}

  \item{...}{extra parameters passed to the Octave function
  \code{exist}.}
}
\description{
  Checks if an Octave object of a given name exists, using
  the Octave function \code{exist}.
}
\section{Octave Documentation for \emph{exist}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(exist,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}


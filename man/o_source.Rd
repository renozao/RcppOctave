\name{o_source}
\alias{o_source}
\title{Sourcing Octave/Matlab Files}
\usage{
  o_source(file = "", text = NULL, sep = ";\\n")
}
\arguments{
  \item{file}{the path to the Octave/Matlab source file --
  typically with extension ".m".}

  \item{text}{a character vector containing \emph{Octave}
  statements, that are concatenated in a temporary file,
  which is then sourced. This argument typically enables
  the evaluation of multiple statements, as opposed to
  single statement evaluation performed by
  \code{\link{o_eval}}.}

  \item{sep}{single character string added as suffix to
  each element of \code{text}. The concatenation of all
  suffixed element should form a valid \emph{Octave}
  block.}
}
\value{
  None
}
\description{
  This function sources an Octave file within the current
  Octave session. The loaded functions are accessible by
  subsequent calls of \code{\link{.CallOctave}}.
}
\section{Octave Documentation for \emph{source}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(source,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
\dontshow{ o_clear() }

# source file
mfile <- system.file("scripts/ex_source.m", package='RcppOctave')
o_source(mfile)

# pass multiple statements
o_source(text="a=1;b=3;c=randn(1,5);")
o_get('a','b','c')

# also works with a character vector of statements
o_source(text=c("a=10;b=30;", "c=randn(1,5)", "d=4"))
o_get('a','b','c', 'd')
}
\seealso{
  Other Octave_files: \code{\link{o_addpath}},
  \code{\link{o_inpath}}
}


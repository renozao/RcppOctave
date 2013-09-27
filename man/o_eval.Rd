\name{o_eval}
\alias{o_eval}
\title{Evaluate an Octave Expression}
\usage{
  o_eval(..., CATCH, unlist = TRUE)
}
\arguments{
  \item{...}{The Octave expression(s) to evaluate, as a
  character string.}

  \item{CATCH}{The Octave expression(s) to evaluate if the
  evaluation(s) of \code{...} fails. See section
  \emph{Octave Documentation} for more details.}

  \item{unlist}{a logical that specifies it single
  variables should be returned as a single value (default),
  or as a list.}
}
\value{
  the result of the evaluation
}
\description{
  Evaluates an Octave expression in the current embedded
  Octave session. The variables assigned in the expression
  are available for subsequent \code{o_eval} calls.
}
\section{Octave Documentation for \emph{evalin}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(evalin,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
# assign some variable
o_eval("a=10")

# retrieve its value in a subsequent call
o_eval("a")
\dontshow{ stopifnot( identical(o_eval("a"), 10) ) }
o_get('a')

# use its value
o_eval("b = a^2")
\dontshow{ stopifnot( identical(o_eval("b = a^2"), 100) ) }

# multiple expression can be evaluated
o_eval(a="10^3", singular="svd(rand(4,4))", random="rand(10, 1)")
# or from a list
l <- list(a="10^3", singular="svd(rand(4,4))", random="rand(10, 1)")
o_eval(l)

# if the evaluation fails then an error is thrown
\dontrun{ o_eval("a=svd()") }

# except if argument CATCH is provided
o_eval("a=svd()", CATCH="a=2")
\dontshow{ stopifnot( identical(o_eval("a"), 2) ) }
}


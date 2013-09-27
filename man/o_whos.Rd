\name{o_whos}
\alias{o_whos}
\title{Detailed Listing of Octave Variables}
\usage{
  o_whos(..., options, rm.ans = FALSE)
}
\arguments{
  \item{...}{filtering patterns or extra arguments passed
  to \code{o_who} and \code{o_whos}. Only names matching
  any of the patterns are returned.}

  \item{options}{options passed to Octave function
  \code{who}. See section \emph{Octave Documentation}.}

  \item{rm.ans}{a logical that indicates if the automatic
  Octave variable \code{ans} should be included in the
  result (\code{FALSE}) or removed (\code{TRUE}).}
}
\description{
  The function \code{o_whos} returns a detailed description
  of the variables defined in the current Octave session.
}
\section{Octave Documentation for \emph{whos}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(whos,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
\dontshow{ o_clear() }

.O$a <- 1
.O$b <- 10
o_whos()

o_eval("sqrt(2)")
o_whos()
}
\seealso{
  Other listoct: \code{\link{o_ls}}, \code{\link{o_who}}
}


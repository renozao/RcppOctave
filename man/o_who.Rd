\name{o_who}
\alias{o_who}
\title{Listing Octave Variables}
\usage{
  o_who(..., options, rm.ans = FALSE, unique = TRUE)
}
\arguments{
  \item{...}{filtering patterns or extra arguments passed
  to \code{o_who} and \code{o_whos}. Only names matching
  any of the patterns are returned.}

  \item{rm.ans}{a logical that indicates if the automatic
  Octave variable \code{ans} should be included in the
  result (\code{FALSE}) or removed (\code{TRUE}).}

  \item{options}{options passed to Octave function
  \code{who}. See section \emph{Octave Documentation}.}

  \item{unique}{a logical that indicates whether unique
  names should be returned. This argument is relevant in
  the case multiple patterns are specified in \code{...}.}
}
\value{
  None
}
\description{
  Lists currently defined variables in Octave global
  context.
}
\section{Octave Documentation for \emph{who}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(who,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
\dontshow{ o_clear() }
o_who()
l <- as.list(setNames(1:10, letters[1:10]))
o_assign(l)
o_who()
\dontshow{  stopifnot( identical(o_who(), names(l)) ) }

prefnames <- paste('pref', letters[1:10], sep='')
o_assign( setNames(l, prefnames) )
o_who()
o_who('pref*')
\dontshow{  stopifnot( identical(o_who('pref*'), prefnames) ) }
}
\seealso{
  Other listoct: \code{\link{o_ls}}, \code{\link{o_whos}}
}


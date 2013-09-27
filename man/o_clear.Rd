\name{o_clear}
\alias{o_clear}
\alias{o_rm}
\title{Deleting Octave Variables}
\usage{
  o_clear(..., all = FALSE, options)

  o_rm(..., all = FALSE, options)
}
\arguments{
  \item{...}{names or pattern of the variables to delete,
  as character strings.}

  \item{all}{a logical indicating whether all user-defined
  objects should be deleted. See section \emph{Octave
  Documentation} for details.}

  \item{options}{options passed to Octave function
  \code{clear}. See section \emph{Octave Documentation}.}
}
\value{
  None
}
\description{
  Deletes variables from Octave global context.

  The function \code{o_rm} is an alias to \code{o_clear}.
}
\section{Octave Documentation for \emph{clear}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(clear,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
\dontshow{ o_clear() }
# Assign a variable in Octave
o_assign('a', 10)
o_who()
\dontshow{ identical(o_who(), 'a') }
# Clear
o_clear()
o_who()
\dontshow{ identical(o_who(), character()) }

# Assign other variables in Octave
.O$a <- 10
.O$b <- 100
.O$ba <- 1000
o_who()
o_get()
\dontshow{ identical(o_who(), c('a', 'b', 'ba')) }

# Clear variable starting with 'b'
o_clear('b*')
o_who()
\dontshow{ identical(o_who(), 'a') }
}


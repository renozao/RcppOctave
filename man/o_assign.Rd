\name{o_assign}
\alias{o_assign}
\alias{o_assignin}
\alias{o_get}
\title{Assign/Get Octave Variables}
\usage{
  o_assign(...)

  o_assignin(...)

  o_get(..., unlist = TRUE, rm.ans = TRUE, pattern)
}
\arguments{
  \item{...}{variables to assign in Octave global context
  for \code{o_assign} , or object names to retrieve from
  Octave for \code{o_get}.}

  \item{unlist}{a logical that specifies it single
  variables should be returned as a single value (default),
  or as a list.}

  \item{rm.ans}{a logical that indicates if the automatic
  Octave variable \code{ans} should be included in the
  result. Default is not to include it unless otherwise
  explicitly specified with this argument, or if it is part
  of the requested variables in \code{...}. When present,
  argument \code{rm.ans} is always honoured.}

  \item{pattern}{regular expression used to filter the
  requested variable names. Only names matching the pattern
  are returned.}
}
\value{
  \code{o_assign} returns invisibly the names of the
  assigned variables.

  \code{o_get} returns a list of the retrieved
  variable/object. If \code{unlist=TRUE} and a single --
  not re-named -- variable/object is requested then only
  its value is returned.
}
\description{
  \code{o_assign} assigns a variable in Octave.
  \code{o_assignin} is an alias for \code{o_assign}.

  \code{o_get} fetches Octave variables/functions and
  possibly rename them on the fly with the provided
  argument names when present. Functions are returned as
  objects of class \code{\linkS4class{OctaveFunction}},
  that can be called subsequently (see the examples).
}
\details{
  \code{o_assign} assigns the variables using the
  arguments' names if present. Variables can also be
  specified as a single named list or environment. Single
  variable assignments can also be specified as
  \code{o_assign('a', 10)}. See \emph{Examples} for more
  details.
}
\note{
  The function \code{o_get} is the equivalent of R
  \code{\link{get}} function and is not related to the
  Octave function \code{get} which returns graphics'
  properties.
}
\section{Octave Documentation for \emph{assignin}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(assignin,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
\dontshow{o_clear()}

## directly assign variables
o_assign(a=1, b=2, c=matrix(1:9, 3))
# retrieve their values
o_get()
\dontshow{ stopifnot( identical(o_get(), list(a=1, b=2, c=matrix(1:9, 3))) ) }

## assign a variable for each element in a list
x <- list(a=10, b=20, c=matrix(101:109, 3))
o_assign(x)
o_get()
\dontshow{ stopifnot( identical(o_get(), x) ) }

## assign the content of an environment
e <- list2env(setNames(x, paste('env', names(x), sep='_')))
o_assign(e)
o_get(pattern="^env_")
\dontshow{o_clear();}

# get all currently defined variables
o_get()

# by default, the automatic variable `ans` is not returned but might be there
# from unstored previous computation
o_eval('svd(rand(3,3))')
o_get()
o_get(rm.ans=FALSE)

# load some variables
x <- list(b=1, c=3, d=matrix(1:9, 3))
o_assign(x)

# re-fetch all variables
o_get()
\dontshow{ stopifnot( identical(o_get(), x) ) }

# only fetch specific variables
o_get('b')
o_get('b', 'c')
# one can rename variables on the fly
o_get(a='b', 'c')
o_get(c(othername='b'))
o_get(c(othername='b', 'c'))

# fetching using a regular expression
o_assign( setNames(1:3, paste("test", 1:3, sep='_')))
o_get()
o_get(pattern="^test")

# works with functions
f <- o_get('svd')
f
f(matrix(1:9,3))
f(matrix(1:9,3), argout=3)

# an error is thrown in the case of multiple matches (the alternatives are shown)
o_clear()
o_assign(aaa=1, ab=2)
try(o_get('a'))
}


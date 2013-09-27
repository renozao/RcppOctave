\docType{data}
\name{.O}
\alias{.O}
\alias{[[,Octave-method}
\alias{$<-,Octave-method}
\alias{$,Octave-method}
\title{Direct Interface to Octave}
\format{\code{.O} is an object of class \code{\linkS4class{Octave}}.}
\usage{
  .O

  \S4method{$}{Octave}(x, name)

  \S4method{[[}{Octave}(x, i, exact = TRUE)

  \S4method{$}{Octave}(x, name) <- value
}
\arguments{
  \item{exact}{logical not used.}

  \item{x}{ object from which to extract element(s) or in
  which to replace element(s).  }

  \item{name}{ A literal character string or a \link{name}
  (possibly \link{backtick} quoted).  For extraction, this
  is normally (see under \sQuote{Environments}) partially
  matched to the \code{\link{names}} of the object.  }

  \item{i}{ indices specifying elements to extract or
  replace.  Indices are \code{numeric} or \code{character}
  vectors or empty (missing) or \code{NULL}.  Numeric
  values are coerced to integer as by
  \code{\link{as.integer}} (and hence truncated towards
  zero).  Character vectors will be matched to the
  \code{\link{names}} of the object (or for
  matrices/arrays, the \code{\link{dimnames}}): see
  \sQuote{Character indices} below for further details.

  For \code{[}-indexing only: \code{i}, \code{j},
  \code{\dots} can be logical vectors, indicating
  elements/slices to select.  Such vectors are recycled if
  necessary to match the corresponding extent.  \code{i},
  \code{j}, \code{\dots} can also be negative integers,
  indicating elements/slices to leave out of the selection.

  When indexing arrays by \code{[} a single argument
  \code{i} can be a matrix with as many columns as there
  are dimensions of \code{x}; the result is then a vector
  with elements corresponding to the sets of indices in
  each row of \code{i}.

  An index value of \code{NULL} is treated as if it were
  \code{integer(0)}.  }

  \item{value}{typically an array-like \R object of a
  similar class as \code{x}.}
}
\description{
  \code{RcppOctave} provides a simple interface to Octave
  via the object \code{.O}, an instance of class
  \code{Octave}, that allows for direct access to Octave
  functions and variables using calls such as:
  \code{.O$svd(matrix(1:9,3))}.
}
\section{Methods}{
  \describe{

  \item{[[}{\code{signature(x = "Octave")}: The method
  \code{[[} provides an alternative way of retrieving
  Octave objects, and is equivalent to \code{o_get(name)}.
  }

  \item{$}{\code{signature(x = "Octave")}: The method
  \code{$} provides a direct way of calling Octave
  functions or retrieving variables from Octave base
  context, via e.g. \code{.O$svd(x)} or \code{.O$a}. It is
  equivalent to \code{o_get(name)}. }

  \item{$<-}{\code{signature(x = "Octave")}: The method
  \code{$<-} allow to directly assign/set Octave variables
  via e.g. \code{.O$a <- 10}. }

  }
}
\examples{
.O
# assign/get Octave variables
.O$a <- 10
.O$a

# call Octave functions
.O$help()
.O$svd(matrix(runif(9), 3))
}
\seealso{
  \code{\link{o_get}}

  \code{\link{o_get}}
}
\keyword{datasets}
\keyword{methods}


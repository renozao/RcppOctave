\docType{class}
\name{OctaveFunction-class}
\alias{OctaveFunction}
\alias{OctaveFunction-class}
\alias{show,OctaveFunction-method}
\title{Wrapping and Defining Octave Functions from R}
\usage{
  OctaveFunction(fun, check = TRUE)

  \S4method{show}{OctaveFunction}(object)
}
\arguments{
  \item{fun}{the name of an existing Octave function or,
  Octave code that defines a function.}

  \item{check}{logical that indicates if the existence of
  the Octave function should be checked. If function does
  not exist then, an error or a warning is thrown if
  \code{check=TRUE} or \code{check=FALSE} respectively. The
  existence check can be completly disabled with
  \code{check=NA}.}

  \item{object}{Any R object}
}
\description{
  Wrapping and Defining Octave Functions from R

  \code{OctaveFunction} objects can be created from
  existing Octave function using their name, or directly
  from their Octave implementation. In this case, the
  Octave code is parsed to extract and use the name of the
  first function defined therein.
}
\section{Slots}{
  \describe{

  \item{name}{name of the wrapped Octave function}

  }
}
\examples{
osvd <- OctaveFunction('svd')
osvd
osvd(matrix(1:9,3))

orand <- OctaveFunction('rand')
orand()
orand(2)
orand(2, 3)

# From source code
myfun <- OctaveFunction('function [Y] = somefun(x)
	Y = x * x;
	end
')
myfun
myfun(10)
}
\keyword{methods}


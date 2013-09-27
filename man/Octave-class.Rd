\docType{class}
\name{Octave-class}
\alias{Octave-class}
\title{Class Octave: Seamless Access to Octave Functions and Variables}
\description{
  This class provides a direct interface to Octave
  functions and base context (i.e. the scope where user
  objects are defined).
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

  \item{.DollarNames}{\code{signature(x = "Octave")}:
  Auto-completion for \code{\linkS4class{Octave}} objects }

  \item{show}{\code{signature(object = "Octave")}: Show
  method for \code{\linkS4class{Octave}} objects }

  }
}
\keyword{internal}


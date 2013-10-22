\name{.CallOctave}
\alias{.CallOctave}
\title{Calling an Octave Function}
\usage{
  .CallOctave(.NAME, ..., argout = -1,
    unlist = !is.character(argout), buffer.std = 3L)
}
\arguments{
  \item{.NAME}{an Octave function name. The function must
  be a valid function name in the current Octave session.}

  \item{...}{arguments passed to the Octave function}

  \item{argout}{the number of output values, or a vector of
  names to use as output variable names. The names are
  directly used and applied to the result list in their
  original order.

  The default value \code{argout=-1} returns: \itemize{
  \item all output values when their number can be
  determined. This would typically be the case for
  functions defined in .m files. Please do read section
  \emph{Details} for considerations about the functions
  that use varargout. \item only the first one otherwise.
  }}

  \item{unlist}{a logical that specifies if an output list
  of length one should be simplified and returned as a
  single value or kept as a list. The default is to unlist
  unless output names were passed in \code{argout}.}

  \item{buffer.std}{logical that indicates if Octave stdout
  or stderr should be buffered. If \code{TRUE}
  output/errors/warnings are displayed at the end of the
  computation. If \code{FALSE} they are directly displayed
  by Octave. It is also possible to selectively buffer
  either one of stdout or stderr, via the following integer
  codes: \itemize{ \item \code{0}: no buffering; \item
  \code{1} or \code{-2}: only stdout is buffered; \item
  \code{2} or \code{-1}: only stderr is buffered; \item
  \code{3}: both stdout and stderr are buffered. }}
}
\value{
  the value returned by the Octave function -- converted
  into standard R objects.
}
\description{
  \code{.CallOctave} calls an Octave function and returns
  its value.
}
\examples{
# data matrix
x <- matrix(1:9, 3)

# call Octave function 'svd': equivalent to [S] = svd(x). See o_help(svd)
.CallOctave('svd', x)

# call Octave function 'svd' asking for 3 output values: [U, S, V] = svd(x)
.CallOctave('svd', x, argout=3)

# call Octave function 'svd' asking for 3 named output values: [U, S, V] = svd(x)
.CallOctave('svd', x, argout=c('U', 'S', 'V'))
}


\name{check.equal}
\alias{check.equal}
\title{Compare Lists or Environments}
\usage{
  check.equal(x, y, msg)
}
\arguments{
  \item{x}{a \code{list} or an \code{environment}}

  \item{y}{a \code{list} or an \code{environment}}

  \item{msg}{a character string used (if not missing) in a
  message that is printed before the comparison. It is
  useful for separating multiple sequential comparisons.}
}
\value{
  No value is returned, but prints out: \itemize{ \item the
  element/variable names of each input list or environment,
  \item the result of the comparison of the elements in
  \code{x} and the corresponding element in \code{y} -- if
  present. }
}
\description{
  This function compares two lists or environments. It is
  useful for comparing results obtained in R and Octave.
}
\examples{
X <- matrix(1:64, 8)
ref <- svd(X)
res <- .O$svd(X, argout=3)

check.equal(ref, res, "R and Octave function 'svd'")
}


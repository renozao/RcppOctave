\name{as.mfile}
\alias{as.mfile}
\title{M Files}
\usage{
  as.mfile(..., pattern = "mfile_", dir = tempdir())
}
\arguments{
  \item{...}{specification of a .m files as character
  arguments. The elements of the vector can be either file
  paths or plain Octave/Matlab code, which are then written
  to disk in -- temporary -- .m files. Note that the paths
  do not need to correspond to existing files.}

  \item{dir}{existing directory where to write the .m files
  generated from the plain code elements of \var{x}.}

  \item{pattern}{a non-empty character vector giving the
  initial part of the name.}
}
\description{
  \code{as.mfile} converts source code or .m filenames into
  real paths to .m files that can be sourced with
  \code{\link[RcppOctave]{o_source}}.
}
\examples{
f <- as.mfile('test.m')
f

# detected code elements are written into temporary files
f <- as.mfile('test.m', "function [y] = myfun()
y = 1;
end
")

\dontrun{
file.show(f[2])
}

# remove all files
unlink(f)
}


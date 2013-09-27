\name{as.mfile}
\alias{as.mfile}
\alias{system.mfile}
\title{M Files}
\usage{
  as.mfile(..., pattern = "mfile_", dir = tempdir())

  system.mfile(..., package = "base")
}
\arguments{
  \item{...}{specification of a .m files as character
  arguments. The elements of the vector can be either file
  paths or plain Octave/Matlab code, which are then written
  to disk in -- temporary -- .m files. Note that the paths
  do not need to correspond to existing files.}

  \item{dir}{existing directory where to write the .m files
  generated from the plain code elements of \var{x}.}

  \item{...}{arguments passed to
  \code{\link{system.file}}.}

  \item{pattern}{a non-empty character vector giving the
  initial part of the name.}

  \item{package}{a character string with the name of a
  single package.  An error occurs if more than one package
  name is given.}
}
\description{
  \code{as.mfile} converts source code or .m filenames into
  real paths to .m files that can be sourced with
  \code{\link[RcppOctave]{o_source}}.

  \code{system.mfile} is a shortcut for:
  \samp{system.file('m-files', ..., package = package)} As
  such it returns empty strings if the requested file does
  not exist. If no arguments besides \code{package} are
  passed, it returns the full path to the package's
  sub-directory \emph{m-files/} -- if it exists.
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


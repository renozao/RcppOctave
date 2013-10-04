\name{system.mfile}
\alias{system.mfile}
\title{Path to Package M-files Standard Location}
\usage{
  system.mfile(..., package = "base")
}
\arguments{
  \item{...}{arguments passed to
  \code{\link{system.file}}.}

  \item{package}{a character string with the name of a
  single package.  An error occurs if more than one package
  name is given.}
}
\description{
  \code{system.mfile} returns paths to .m files installed
  with packages.
}
\details{
  \code{system.mfile} is a shortcut for:
  \samp{system.file('m-files', ..., package = package)} As
  such it returns empty strings if the requested file does
  not exist. If no arguments besides \code{package} are
  passed, it returns the full path to the package's
  sub-directory \emph{m-files/} -- if it exists.
}


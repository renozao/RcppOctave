\docType{data}
\name{Octave.version}
\alias{Octave.config}
\alias{Octave.info}
\alias{Octave.version}
\title{Octave Configuration and Installation Information}
\format{List of 4
 $ platform      : chr "x86_64-pc-linux-gnu"
 $ version       : chr "3.8.1"
 $ api           : chr "api-v49+"
 $ version.string: chr "Octave version 3.8.1 (api-v49+)"
 - attr(*, "class")= chr "simple.list"}
\usage{
  Octave.version

  Octave.config

  Octave.info(name)
}
\arguments{
  \item{name}{name of the variable to retrieve}
}
\description{
  The functions documented here enable retrieving
  information about the Octave installation used at
  installation or runtime -- which should normally be the
  same.

  \code{Octave.info} is a function that retrieves
  information about the version of Octave that is used by
  the current session of \pkg{RcppOctave}.
}
\details{
  \code{Octave.version} is list that contains version
  information as determined by the configure script at
  installation time.

  \code{Octave.config} is a list that extends
  \code{Octave.version} with extra information about
  compilers and compilation flags.
}
\examples{
\dontshow{# roxygen generated flag
options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
}

#----------
# Octave.version
#----------
Octave.version
Octave.config

#----------
# Octave.info
#----------
Octave.info()
Octave.info('modules')
}
\seealso{
  Other Octave.info: \code{\link{o_config_info}}, \code{\link{octave_config}}, \code{\link{Octave.home}}
}
\keyword{datasets}


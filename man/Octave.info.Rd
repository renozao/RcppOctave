\name{Octave.info}
\alias{Octave.info}
\title{Octave Installation Details}
\usage{
  Octave.info(name)
}
\arguments{
  \item{name}{name of the variable to retrieve}
}
\description{
  Retrieves information about the version of Octave that is
  used by the current session of \pkg{RcppOctave}.
}
\examples{
Octave.info()
Octave.info('modules')
}
\seealso{
  Other Octave.info: \code{\link{o_config_info}},
  \code{\link{octave_config}}, \code{\link{Octave.home}}
}


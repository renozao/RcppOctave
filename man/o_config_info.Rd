\name{o_config_info}
\alias{o_config_info}
\title{Embedded Octave Configuration and Installation Information}
\usage{
  o_config_info(var = c("CC", "CC_VERSION", "FC"))
}
\arguments{
  \item{var}{name of the configuration variable to
  retrieve.}
}
\description{
  Retrieves configuration and installation information
  about the embedded instance of Octave currently used by
  \pkg{RcppOctave}.
}
\examples{
o_config_info()
o_config_info('USE_64_BIT_IDX_T')
}
\seealso{
  Other Octave.info: \code{\link{octave_config}},
  \code{\link{Octave.config}}, \code{\link{Octave.home}},
  \code{\link{Octave.info}}, \code{\link{Octave.version}}
}


\name{octave_config}
\alias{octave_config}
\title{Octave Utils: octave-config}
\usage{
  octave_config(varname, verbose = FALSE, warn = TRUE,
    mustWork = TRUE, bindir = Octave.home("bin"))
}
\arguments{
  \item{varname}{Name (as a character string) of the Octave
  configuration variable to retrieve. It is used in
  following system call \samp{octave-config -p <varname>}.
  This function is vectorised and returns a character
  vector of the same length as its argument.}

  \item{warn}{logical that indicates if a warning should be
  thrown when a variable is returned empty, which generally
  means that \code{x} is not a valid config variable name.}

  \item{mustWork}{logical that indicates if an error should
  be thrown if failing to load Octave configuration.}

  \item{bindir}{path to Octave bin/ sub-directory where to
  look for \code{octave-config}. If \code{NULL} or
  \code{NA}, then the system PATH is looked up.}
}
\description{
  Retrieves Octave configuration variables using
  \code{octave-config}.
}
\details{
  \code{octave_config} uses the \code{octave-config}
  utility binary shipped with Octave to query details about
  the local Octave installation. Failure to retrieve such
  information is generally due to the binary not being
  found. By default, it is looked up in the \code{bin/}
  sub-directory of the path returned by
  \code{\link{Octave.home}()}.
}
\examples{
octave_config('VERSION')
octave_config('BINDIR')
}
\seealso{
  Other Octave.info: \code{\link{o_config_info}},
  \code{\link{Octave.config}}, \code{\link{Octave.home}},
  \code{\link{Octave.info}}, \code{\link{Octave.version}}
}


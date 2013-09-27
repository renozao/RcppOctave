\name{ostart}
\alias{oconfig}
\alias{oend}
\alias{omodules}
\alias{ostart}
\alias{overbose}
\title{Low-level Function Interfacing with Octave}
\usage{
  ostart(verbose = FALSE, warnings = FALSE)

  oend()

  overbose(value)

  oconfig(varname, verbose = FALSE, warn = TRUE)

  omodules(verbose = getOption("verbose"))
}
\arguments{
  \item{verbose}{logical value used as the inital verbosity
  status.}

  \item{warnings}{logical that indicates if Octave startup
  warnings should be shown.}

  \item{value}{logical value to toggle verbosity}

  \item{varname}{Name (as a character string) of the Octave
  configuration variable to retrieve. It is used in
  following system call \samp{octave-config -p <varname>}.
  This function is vectorised and returns a character
  vector of the same length as its argument.}

  \item{warn}{logical that indicates if a warning should be
  thrown when a variable is returned empty, which generally
  means that \code{x} is not a valid config variable name.}
}
\description{
  \code{ostart} Initialize an Octave session.

  \code{oend} clears and terminates the current Octave
  session.

  \code{overbose} toggles the verbosity of RcppOctave
  calls: messages tracks any function call, or conversion
  of objects between R and Octave (e.g. arguments and
  results).

  \code{oconfig} retrieves Octave configuration variables.

  \code{omodules} add the Octave modules shipped with
  RcppOctave to Octave path.
}
\seealso{
  OctaveConfig
}


\name{ostart}
\alias{oconfig}
\alias{oend}
\alias{omodules}
\alias{ostart}
\alias{overbose}
\title{Low-level Function Interfacing with Octave}
\usage{
  ostart(verbose = FALSE, warnings = FALSE, force = FALSE)

  oend()

  overbose(value)

  oconfig(varname, verbose = FALSE, warn = TRUE,
    mustWork = TRUE)

  omodules(verbose = getOption("verbose"))
}
\arguments{
  \item{verbose}{logical value used as the inital verbosity
  status.}

  \item{warnings}{logical that indicates if Octave startup
  warnings}

  \item{force}{logical that indicates if Octave session
  should be reinitialised, even if one was previously
  started (not meant to be used by end-users). should be
  shown.}

  \item{value}{logical value to toggle verbosity}

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
}
\description{
  \code{ostart} Initialize an Octave session.

  \code{oend} clears and terminates the current Octave
  session.

  \code{overbose} toggles the verbosity of RcppOctave
  calls: messages tracks any function call, or conversion
  of objects between R and Octave (e.g. arguments and
  results).

  Failure to load Octave configuration is generally due to
  Octave binaries not being found. Users should ensure that
  Octave binary directory is in the PATH or set the option
  'octave.path' to the path to the relevant directory.

  \code{omodules} add the Octave modules shipped with
  RcppOctave to Octave path.
}
\seealso{
  OctaveConfig
}


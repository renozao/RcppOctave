\name{octave_start}
\alias{octave_config}
\alias{octave_end}
\alias{octave_modules}
\alias{octave_start}
\alias{octave_verbose}
\title{Low-level Function Interfacing with Octave}
\usage{
  octave_start(verbose = FALSE, warnings = FALSE,
    force = FALSE)

  octave_end()

  octave_verbose(value)

  octave_config(varname, verbose = FALSE, warn = TRUE,
    mustWork = TRUE, bindir = getOption("octave.path"))

  octave_modules(verbose = getOption("verbose"))
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

  \item{bindir}{path to Octave bin/ sub-directory where to
  look for \code{octave-config}. If \code{NULL} or
  \code{NA}, then the system PATH is used.}
}
\description{
  \code{octave_start} Initialize an Octave session.

  \code{octave_end} clears and terminates the current
  Octave session.

  \code{octave_verbose} toggles the verbosity of RcppOctave
  calls: messages tracks any function call, or conversion
  of objects between R and Octave (e.g. arguments and
  results).

  \code{octave_config} uses the \code{octave-config} system
  utility that ships with Octave to query details about the
  local Octave installation. Failure to load Octave
  configuration is generally due to this Octave binary not
  being found in the system PATH. Users should ensure that
  the PATH contains Octave binary directory path.
  Alternatively one may use option 'octave.path' to specify
  the directory where to find \code{octave-config}:

  \code{octave_modules} add the Octave modules that ship
  with RcppOctave to Octave loading path.
}
\details{
  \samp{options(octave.path =
  '/absolute/path/to/octave/bin')}

  Note that in this case, the system PATH is not used.
}
\seealso{
  OctaveConfig
}


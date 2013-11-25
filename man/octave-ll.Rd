\name{octave_start}
\alias{octave_end}
\alias{octave_modules}
\alias{octave_start}
\alias{octave_verbose}
\title{Low-level Function Interfacing with Octave}
\usage{
  octave_start(verbose = FALSE, warnings = FALSE,
    force = FALSE)

  octave_end(verbose = getOption("verbose"))

  octave_verbose(value)

  octave_modules(verbose = getOption("verbose"))
}
\arguments{
  \item{verbose}{logical that toggle verbosity. In
  \code{octave_start}, it is the value used as the initial
  global verbosity state. If \code{TRUE} all calls and
  conversions between R and Octave produce diagnostic
  messages.}

  \item{warnings}{logical that indicates if Octave startup
  warnings}

  \item{force}{logical that indicates if Octave session
  should be reinitialised, even if one was previously
  started (not meant to be used by end-users). should be
  shown.}

  \item{value}{logical value to toggle verbosity}
}
\description{
  \code{octave_start} Initialize an Octave session.

  \code{octave_end} clears and terminates the current
  Octave session.

  \code{octave_verbose} toggles the verbosity of RcppOctave
  calls: messages tracks any function call, or conversion
  of objects between R and Octave (e.g. arguments and
  results).

  \code{octave_modules} add the Octave modules that ship
  with RcppOctave to Octave loading path.
}


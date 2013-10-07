\name{OctaveInit}
\alias{OctaveInit}
\title{Initialise Octave}
\usage{
  OctaveInit()
}
\description{
  Starts an Octave session through the \pkg{RcppOctave}
  package.
}
\details{
  This function is normally not meant to be called by the
  end-user, except when the \pkg{RcppOctave} package fails
  to detect Octave configuration when first loading.
  Calling \code{OctaveInit} after correctly setting the
  path or option \code{'octave.path'} should fix the issue,
  and start a fresh Octave session.

  If an Octave session has already successfully started,
  then this function has no effect.
}


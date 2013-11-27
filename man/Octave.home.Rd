\name{Octave.home}
\alias{Octave.home}
\title{Octave Home Directory}
\usage{
  Octave.home(..., configure = FALSE, use.system = TRUE)
}
\arguments{
  \item{...}{character strings that are appended to Octave
  path via \code{\link{file.path}}.}

  \item{configure}{logical that indicates if one should
  directly return the path that was used when configuring
  (i.e. installing) \pkg{RcppOctave}}

  \item{use.system}{logical that indicates if one should
  try using \code{octave-config} to retrieve Octave home
  directory. This would be done as a last resort, if the
  path could not be determined in any other ways (see
  \emph{Details}).}
}
\value{
  a character string, or \code{NULL} if the path was not
  found.
}
\description{
  Returns the path to Octave home directory, i.e. the
  directory that contains the \code{bin/} sub-directory
  where Octave binaries can be found, e.g., typically
  \code{/usr/} on Linux machines.
}
\details{
  The path to Octave home directory is determined in the
  following order:

  \itemize{ \item value of global option
  \code{'octave.home'}; \item value of the environment
  variable \code{'OCTAVE_HOME'}; \item path used during
  configuration/installation of \pkg{RcppOctave}. \item
  path returned by \code{octave-config}, which is itself
  looked up in the system PATH. }

  If set, the global option or environment variable should
  contain the \strong{absolute} path to Octave root
  directory.
}
\examples{
Octave.home()
}
\seealso{
  Other Octave.info: \code{\link{o_config_info}},
  \code{\link{octave_config}}, \code{\link{Octave.config}},
  \code{\link{Octave.info}}, \code{\link{Octave.version}}
}


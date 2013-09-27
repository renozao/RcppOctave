\name{o_addpath}
\alias{o_addpath}
\alias{o_inpath}
\title{Manipulating Octave Search Path}
\usage{
  o_addpath(DIR1, ..., OPTION = "-begin")

  o_inpath(...)
}
\arguments{
  \item{DIR1}{path specification to add to Octave search
  path. See section \emph{Octave Documentation}.}

  \item{...}{other path specifications}

  \item{OPTION}{option that specifies how the path should
  be added. Possible values are: \code{'-begin', 0, '-end',
  1}. See section \emph{Octave Documentation}.}
}
\value{
  returns invisibly the old value of search path.
}
\description{
  Adds a directory at the beginning of Octave search path.

  \code{o_inpath} tells if a directory or files are in
  Octave path.
}
\details{
  The .oct files present in directories from the search
  path are looked up when an object or function is
  requested but not loaded in the current session. The
  files are watched and automatically reloaded in case
  modification.
}
\section{Octave Documentation for \emph{addpath}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(addpath,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
# call an undefined function
try(.CallOctave('fun1'))

# add to the path a directory with a .oct file that contains a definition for 'fun1'
o_addpath(system.file('scripts', package='RcppOctave'))

# re-call the function
#.CallOctave('fun1')

# change the .oct file
o_addpath(tempdir())
o_inpath(tempdir())
o_inpath(tempfile())
}
\seealso{
  Other Octave_files: \code{\link{o_source}}
}


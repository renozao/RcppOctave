\name{sourceExamples}
\alias{sourceExamples}
\title{Loading Example M-files}
\usage{
  sourceExamples(file)
}
\arguments{
  \item{file}{filename of the example script to source. If
  missing, the function lists all the m-files from the
  \dQuote{scripts/} sub-directory.}
}
\description{
  Source an example M-file in the sub-directory
  \dQuote{scripts/} of RcppOctave installation.
}
\examples{
sourceExamples()
sourceExamples('ex_source.m')
}


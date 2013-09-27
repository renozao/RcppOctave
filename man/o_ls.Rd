\name{o_ls}
\alias{o_ls}
\title{Listing Objects from the Current Octave Session}
\usage{
  o_ls(long = FALSE, rm.ans = TRUE)
}
\arguments{
  \item{rm.ans}{a logical that indicates if the automatic
  Octave variable \code{ans} should be included in the
  result. Default (\code{TRUE}) is not to include it.}

  \item{long}{logical that indicates the desired type of
  output: if \code{FALSE} (default) then only the names of
  the variables are returned (like dispatched
  \code{o_who}), otherwise a list with more detailed
  information about each variable is returned (like
  \code{\link{o_whos}}.}
}
\value{
  a character vector or a list depending on the value of
  argument \code{long}.
}
\description{
  The function \code{o_ls} is an enhanced listing function,
  which also lists user-defined functions, as opposed to
  \code{\link{o_who}} or \code{\link{o_whos}}, which only
  list variables. Note that this function works properly on
  Octave >= 3.6.1, but is known not to list user-defined
  functions on Octave 3.4.1 (for some unknown reason the
  Octave function \code{completion_matches} does not return
  the names of user-defined functions).
}
\examples{
\dontshow{ o_clear(all=TRUE) }

# only variables
o_assign(list(a=1, b=2, c=5))
o_ls()
# compare with the output of standard Octave functions
o_who() # should be the same output
o_whos()

# variables and user-defined functions
o_clear(all=TRUE) # first clear Octave session
o_source(system.file('scripts/ex_source.m', package='RcppOctave'))
o_ls()
o_ls(long=TRUE)
# compare with the output of standard Octave functions
o_who()
o_whos()
}
\seealso{
  Other listoct: \code{\link{o_who}}, \code{\link{o_whos}}
}


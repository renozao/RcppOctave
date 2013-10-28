\name{o_help}
\alias{o_doc}
\alias{o_help}
\title{Accessing Octave Help and Documentation Pages}
\usage{
  o_help(NAME, character.only = FALSE,
    show = interactive(),
    format = c("plain", "rd", "txt", "latex", "HTML"))

  o_doc(FUNCTION_NAME)
}
\arguments{
  \item{NAME}{Octave symbol (e.g. command, function,
  operator) passed to Octave function \code{help} to
  retrieve the related documentation.}

  \item{character.only}{a logical indicating whether
  \code{NAME} can be assumed to be a character string
  (\code{TRUE}) or should be substituted with
  \code{\link{substitute}} before using them (default).}

  \item{show}{logical that specifies if the help page
  should be shown using the as R documentation file
  (default), e.g. using a pager, or only returned as a
  single string. Note that when \code{show=TRUE}, the
  string is still returned but invisibly.}

  \item{format}{a specification of the output format. If
  \code{TRUE} or \code{'rd'}, the result is Rd code that
  wraps the Octave documentation string and is suitable for
  inclusion into Rd files. If one of the strings
  \code{'txt', 'latex'} or \code{'HTML'}, then the result
  is formated using the corresponding Rd conversion
  function from the \pkg{tools} package
  \code{\link{Rd2txt}}, \code{\link{Rd2latex}} or
  \code{\link{Rd2HTML}}.}

  \item{FUNCTION_NAME}{the name of the function from which
  to show the documentation. See the relevant \emph{Octave
  Documentation} section below.}
}
\value{
  this function is usually called for its side effect of
  printing the help page on standard output (argument
  \code{show=TRUE}), but it invisibly returns the help page
  as a single character string.
}
\description{
  \code{o_help} retrieves the Octave help page associated
  with a given symbol. By default the page is printed out,
  but may also be silently retrieved or formatted for
  direct inclusion in R documentation files (i.e. Rd
  files).

  \code{o_doc} displays documentation for the function
  FUNCTION_NAME directly from an on-line version of the
  printed manual, using the GNU Info browser. Type `q` to
  quit the browser.
}
\section{Octave Documentation for \emph{help}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(help,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}

\section{Octave Documentation for \emph{doc}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(doc,
  format='rd')}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
#----------
# o_help
#----------
\dontshow{
	if( interactive() ){
		o_help <- function(..., show=FALSE){
			RcppOctave::o_help(..., show=show)
		}
	}
}

o_help(print)
o_help(rand)
# or equivalently
o_help('rand')

# to include in Rd files, use argument rd=TRUE in an \\Sexpr:
\dontrun{
 \\Sexpr[results=rd,stage=render]{RcppOctave::o_help(rand, format='rd')}
}

# to see the included Rd code
o_help(rand, format=TRUE)
o_help(rand, format='HTML')
o_help(rand, format='latex')

#----------
# o_doc
#----------
o_doc(text)
# or equivalently
o_doc('text')
}


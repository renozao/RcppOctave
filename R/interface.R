# R functions to interact with an embedded Octave instance
# 
# Author: "Renaud Gaujoux"
# Creation: 26 Oct 2011
###############################################################################

#' @include utils.R
NULL

#' Calling an Octave Function
#' 
#' \code{.CallOctave} calls an Octave function and returns its value.
#' 
#' @param .NAME an Octave function name. The function must be a valid function 
#' name in the current Octave session.
#' @param ... arguments passed to the Octave function
#' @param argout the number of output values, or a vector of names to use as output
#' variable names. The names are directly used and applied to the result list in 
#' their original order.
#'  
#' The default value \code{argout=-1} returns:
#' \itemize{
#' \item all output values when their number can be determined. This would typically  
#' be the case for functions defined in .m files. Please do read section 
#' \emph{Details} for considerations about the functions that use varargout. 
#' \item only the first one otherwise.
#' }
#' @param unlist a logical that specifies if an output list of length one 
#' should be simplified and returned as a single value or kept as a list.
#' The default is to unlist unless output names were passed in \code{argout}.
#' @param buffer.stderr logical that indicates if messages sent to stderr should be buffered and
#' displayed at the end of the computation (\code{TRUE}) or as they come (\code{FALSE}).
#' 
#' @return the value returned by the Octave function -- converted into standard 
#' R objects.
#' 
#' @export
#' @examples 
#' 
#' # data matrix
#' x <- matrix(1:9, 3)
#' 
#' # call Octave function 'svd': equivalent to [S] = svd(x). See o_help(svd)
#' .CallOctave('svd', x)
#' 
#' # call Octave function 'svd' asking for 3 output values: [U, S, V] = svd(x)  
#' .CallOctave('svd', x, argout=3)
#' 
#' # call Octave function 'svd' asking for 3 named output values: [U, S, V] = svd(x)
#' .CallOctave('svd', x, argout=c('U', 'S', 'V'))
#' 
.CallOctave <- function(.NAME, ..., argout=-1, unlist=!is.character(argout), buffer.stderr = TRUE){
	
	res <- .Call("octave_feval", .NAME, list(...), argout, unlist, buffer.stderr, PACKAGE='RcppOctave')
	if( identical(argout, 0) || identical(argout, 0L) )	invisible()
	else if( is.null(res) && argout <= 1L ) invisible()
	else res
}

#' Low-level Function Interfacing with Octave
#' 
#' \code{ostart} Initialize an Octave session.
#' 
#' @param verbose logical value used as the inital verbosity status.
#' @param warnings logical that indicates if Octave startup warnings 
#' should be shown.
#' 
#' @rdname octave-ll
#' @export
ostart <- function(verbose=FALSE, warnings = FALSE){
	.Call("octave_start", verbose, warnings, PACKAGE='RcppOctave')
}

#' \code{oend} clears and terminates the current Octave session.
#' 
#' @rdname octave-ll
#' @export
oend <- function(){
	.Call("octave_end", PACKAGE='RcppOctave')
}

#' \code{overbose} toggles the verbosity of RcppOctave calls: messages tracks 
#' any function call, or conversion of objects between R and Octave 
#' (e.g. arguments and results).
#' 
#' @param value logical value to toggle verbosity
#' 
#' @rdname octave-ll
#' @export
overbose <- function(value){
	invisible(.Call("octave_verbose", value, PACKAGE='RcppOctave'))
}

#' \code{oconfig} retrieves Octave configuration variables. 
#' 
#' @param varname Name (as a character string) of the Octave configuration 
#' variable to retrieve. It is used in following system call 
#' \samp{octave-config -p <varname>}.
#' This function is vectorised and returns a character vector of the same length
#' as its argument.
#' @param warn logical that indicates if a warning should be thrown when a 
#' variable is returned empty, which generally means that \code{x} is not a valid 
#' config variable name.
#'  
#' @rdname octave-ll
#' @seealso OctaveConfig
#' @export
oconfig <- function(varname, verbose=FALSE, warn=TRUE){
	sapply(varname, function(x){
		if( verbose ) message("# Loading Octave config variable '", x, "' ... ", appendLF=FALSE)
		res <- system(paste('octave-config -p', x), intern=TRUE)
		if( res == '' ){
			if( verbose ) message("WARNING")
			if( warn ) warning("Octave config variable '", x, "' is empty")
			return(res)
		}
		if( verbose ) message('OK')
		res
	})
}

#' \code{omodules} add the Octave modules shipped with RcppOctave to Octave path.
#' 
#' @rdname octave-ll
#' @export
omodules <- function(verbose=getOption('verbose')){
	
	path <- OctaveConfig('modules')
	if( verbose )
		message("Loading Octave modules for ", packageName()
				, " from '", path, "'");
	o_addpath(path)
}

#' Loading Example M-files
#' 
#' Source an example M-file in the sub-directory \dQuote{scripts/} of RcppOctave
#' installation. 
#' 
#' @param file filename of the example script to source. If missing, the function 
#' lists all the m-files from the \dQuote{scripts/} sub-directory. 
#' 
#' @export
#' @examples 
#' 
#' sourceExamples()
#' sourceExamples('ex_source.m')
#' 
sourceExamples <- function(file){
	if( missing(file) ){
		list.files(packagePath('scripts'), pattern="\\.m$")
	}else{# source script
		o_source(packagePath('scripts', file))
	}
	
}



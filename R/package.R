#' @include utils.R
NULL

#' Interfacing R with Octave.
#' 
#' The primary goal is to facilitate the port of Matlab/Octave scripts to R. 
#' The package enables to call any Octave functions from R and as well as browsing 
#' their documentation, passing variables between R and Octave, using R core RNGs 
#' in Octave, which ensure stochastic computations are also reproducible.
#'
#' \tabular{ll}{
#' Package: \tab RcppOctave\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0\cr
#' Date: \tab 2011-11-01\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @author
#' Renaud Gaujoux \email{renaud@@cbio.uct.ac.za}
#'
#' Maintainer: Renaud Gaujoux \email{renaud@@cbio.uct.ac.za}
#' @name RcppOctave
#' @rdname RcppOctave-package
#' @docType package
#' @title Interfacing R with Octave
#' @keywords package
#' 
#' @cite Eaton2002
#' @bibliography ~/Documents/articles/library.bib
#' @examples
#' 
#' .CallOctave('svd', matrix(1:9, 3))
#' o_help('svd')
#' 
#' @import pkgmaker
#' @import Rcpp
#' @seealso See \code{\link{.CallOctave}}, \code{\link{o_source}}, \code{\link{o_help}}
NULL

## #' @useDynLib RcppOctave

#inlineCxxPlugin <- function (...) 
#{
#	includes <- sprintf("%s\n#include <Rcpp.h>\n%s\n\n#ifndef BEGIN_RCPP\n#define BEGIN_RCPP\n#endif\n\n#ifndef END_RCPP\n#define END_RCPP\n#endif\n\nusing namespace Rcpp;\n", 
#			include.before, include.after)
#	list(env = list(PKG_LIBS = paste(libs, Rcpp:::RcppLdFlags())), 
#			includes = includes, LinkingTo = LinkingTo, body = function(x) {
#				sprintf("BEGIN_RCPP\n%s\nEND_RCPP", x)
#			}, Depends = Depends, Makevars = Makevars, Makevars.win = Makevars.win)
#}

#' Cached RcppOctave Configuration Paths
#' 
#' @param name Name of an RcppOctave path variable
#' @param ... extra names to be concatenated to the result with \code{\link{file.path}}.
#' Only used when \code{name} is not missing.
#' @return  a list (if \code{name is missing}) or a single character string.
#' 
#' @keywords internal
#' @export
#' @examples
#' 
#' OctaveConfig()
#' OctaveConfig('lib')
#' OctaveConfig('include')
#' 
OctaveConfig <- local({
	# config cache
	.OctaveConfig <- NULL
	function(name, ..., reset=FALSE){
	
	# return the whole config list if no name is provided
	if( is.null(.OctaveConfig) || missing(name) || reset ){
		# create the config list at first call
		if( is.null(.OctaveConfig) || reset ){
			conf <- list(lib=oconfig(c('LIBDIR', 'OCTLIBDIR'))
						, include=oconfig(c('INCLUDEDIR', 'OCTINCLUDEDIR'))
				)
			
			# add a configuration variable for the module path
			conf$modules <- packagePath('modules')
			if( pkgmaker::isDevNamespace() ){ # fix module path due changes in devtools compilation step
				conf$modules <- file.path(tempdir(), packageName(), 'modules')
				# create module directory
				if( !file.exists(conf$modules) ){
					message("Faking devtools compilation directory '", conf$modules, "'")					
					dir.create(conf$modules, recursive=TRUE)
					src <- packagePath('src/modules')
					file.copy(file.path(src, c('PKG_ADD', list.files(src, pattern="*.oct$"))), conf$modules)
				}				
			} 
			
			.OctaveConfig <<- conf
		}
		
		if( missing(name) ) return(.OctaveConfig)
	}
		
	settings <- .OctaveConfig[[name]]
	file.path(settings, ...)
	}
})

# Load/Unload Octave Libraries
.OctaveLibs <- function(unload=FALSE){
		
	dyn.fun <- function(x, dlls){
		if( !x %in%  dlls ){
			libname <- paste(x, .Platform$dynlib.ext, sep='')
			libs <- OctaveConfig('lib', libname)
			for( l in libs ){
				if( utils::file_test('-f', l) ){
					return( if( !unload ) dyn.load(l) else dyn.unload(l) )
				}
			}
			stop("Could not find Octave library '", x, "'")
		}
	}
	
	# load/unload required Octave libraries
	octlibs <- c('liboctave', 'liboctinterp')
	sapply(octlibs, dyn.fun, names(base::getLoadedDLLs()))
}

.onLoad <- function(libname, pkgname){
	
	# load Octave configuration
	OctaveConfig()
	
	# load required Octave libraries
	.OctaveLibs()
	
	# load compiled library normally or in devmode
	if( !isDevNamespace() ) library.dynam(packageName(), pkgname, libname)
	else dyn.load(packagePath('src', paste0(packageName(), .Platform$dynlib.ext)))
#	else compile_src() # compile source files and load

	# start Octave session
	ostart()
	# load Octave modules
	omodules()	
}

.onUnload <- function(libpath) {
	
	# unload compiled library normally or in devmode
	dlls <- base::getLoadedDLLs()
	pname <- packageName()
	if ( pname %in%  names(dlls) ){
		if( !missing(libpath) )	library.dynam.unload(pname, libpath)
		else dyn.unload(dlls[[pname]][['path']])
	}
	
	# unload required Octave libraries 
	.OctaveLibs(unload=TRUE)
}


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
#' @param reset logical that indicates if the configuration should be reloaded
#' @param check logical that indicates if the presence of a working version of 
#' the \code{octave-config} binary should be tested before trying to load the
#' configuration. 
#' @return  a list (if \code{name is missing}) or a single character string.
#' 
#' @keywords internal
#' @export
#' @examples
#' 
#' OctaveConfig()
#' OctaveConfig('lib')
#' OctaveConfig('include')
#' OctaveConfig('modules')
#' 
OctaveConfig <- local({
	# config cache
	.OctaveConfig <- NULL
	function(name, ..., reset=FALSE, check = FALSE){
	
    if( check ){
        if( is.null(oconfig('VERSION', mustWork = FALSE, warn = FALSE)) ){
            return()
        }
    }
                    
		            
	# return the whole config list if no name is provided
	if( is.null(.OctaveConfig) || missing(name) || reset ){
        
        # create the config list at first call
		if( is.null(.OctaveConfig) || reset ){
			conf <- list(lib=oconfig(c('BINDIR', 'LIBDIR', 'OCTLIBDIR'))
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
    # settings may contain more than one path => sapply
	unlist(sapply(settings, file.path, ...), use.names = FALSE)
	}
})

# Load/Unload Octave Libraries
.OctaveLibs <- local({
    .libs <- NULL
    function(load = NULL){
    		
        if( is.null(load) ) return( .libs )
    	            
    	dyn.fun <- function(x, dlls){
    		if( !x %in%  dlls ){
                # add platform depend extension
    			libname <- paste(x, .Platform$dynlib.ext, sep='')
                
                # On windows: Octave GCC dll files end with .a
                if( .Platform$OS.type == 'windows' ){
                    libname <- c(libname, paste(x, '-1', .Platform$dynlib.ext, sep = ''))
                }
                #
                
                # look-up and load dll files in the
    			libs <- OctaveConfig('lib', libname)
    			for( l in libs ){
    				if( utils::file_test('-f', l) ){
                        if( isTRUE(load) ) dyn.load(l) else dyn.unload(l)
    					return( l )
    				}
    			}
                # error if none of the expected path exist
    			stop("Could not find Octave library '", x, "' in ", str_out(unique(dirname(libs)), Inf)
                    , ".\n  File(s) looked up: ", str_out(basename(libs), Inf))
    		}
    	}
    	
        # load/unload required Octave libraries
    	octlibs <- c('libcruft', 'liboctave', 'liboctinterp')
        if( isFALSE(load) ) octlibs <- rev(octlibs)
    	.libs <<- sapply(octlibs, dyn.fun, names(base::getLoadedDLLs()))    
    }
})

hook_Sys.path <- local({
    .opath <- NULL 
    .addon <- NULL
    function(dir, snapshot.only = FALSE){
        
        path <- Sys.getenv('PATH')
        spath <- strsplit(path, .Platform$path.sep)[[1]]
        
        if( isTRUE(snapshot.only) ){
            if( !is.null(.opath) ){
                spath.old <- strsplit(.opath, .Platform$path.sep)[[1]]
                .addon <<- setdiff(spath, spath.old)
            }
            return()
        }
        
        if( !is.null(dir) ){
            .opath <<- path
            if( !dir %in% spath ){
                Sys.setenv(PATH = paste(dir, path, sep = .Platform$path.sep))
            }
        }else if( length(.addon) ){
            cpath <- setdiff(spath, .addon)
            Sys.setenv(PATH = paste(cpath, collapse = .Platform$path.sep))
        }
    }
})

#' Initialise Octave
#' 
#' Starts an Octave session through the \pkg{RcppOctave} package.
#' 
#' This function is normally not meant to be called by the end-user, 
#' except when the \pkg{RcppOctave} package fails to detect 
#' Octave configuration when first loading.
#' Calling \code{OctaveInit} after correctly setting the path or option
#' \code{'octave.path'} should fix the issue, and start a fresh 
#' Octave session.
#' 
#' If an Octave session has already successfully started, then this function
#' has no effect.
#' 
#' @export
OctaveInit <- function(){
    .onLoad(NULL, NULL)
}

.onLoad <- function(libname, pkgname){
    
    message <- if( isLoadingNamespace() ) packageStartupMessage else base::message
    # try loading Octave configuration
    if( is.null(OctaveConfig(check = TRUE)) ){
        message("RcppOctave [", utils::packageVersion('RcppOctave') , "] - Octave (not configured)"
                , "\nNOTE: path to Octave binaries should be in the system path or set in option 'octave.path'")
        return()
    }
    
    # display info about config  
    octave_bindir <- oconfig('BINDIR')
    message("RcppOctave [", utils::packageVersion('RcppOctave') , "] - "
            , o_version_string()
            , "\nOctave path: ", octave_bindir)
	
	# load required Octave libraries _before_ loading the package's library
	#.OctaveLibs(TRUE)
    hook_Sys.path(octave_bindir)
	
	# load compiled library normally or in devmode
    if( is.null(pkgname) ) pkgname <- packageName() 
    if( is.null(libname) ) libname <- dirname(path.package(pkgname))
    dlls <- base::getLoadedDLLs()
	if ( !pkgname %in%  names(dlls) ){
    	if( !isDevNamespace() ) library.dynam(pkgname, pkgname, libname)
    	else dyn.load(packagePath('src', paste0(pkgname, .Platform$dynlib.ext)))
    }
    
	# start Octave session
	ostart()
	# load Octave modules
	omodules()	
    
    hook_Sys.path(snapshot.only = TRUE)

}

.onUnload <- function(libpath) {
	
	# unload compiled library normally or in devmode
	dlls <- base::getLoadedDLLs()
	pname <- 'RcppOctave'
	if ( pname %in%  names(dlls) ){
		if( !missing(libpath) )	library.dynam.unload(pname, libpath)
		else dyn.unload(dlls[[pname]][['path']])
	}
	
	# unload required Octave libraries 
	#.OctaveLibs(FALSE)
    # cleanup path
    hook_Sys.path(NULL)
}


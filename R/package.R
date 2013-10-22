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

#' Octave-RcppOctave Configuration
#' 
#' Configures Octave and load RcppOctave  
#' 
#' @param name Name of an RcppOctave path variable
#' @param ... extra names to be concatenated to the result with \code{\link{file.path}}.
#' Only used when \code{name} is not missing.
#' @param path path to Octave bin/ sub-directory
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
	function(name, ..., path=NULL){
		    
        do_init <- FALSE
    	# return the whole config list if no name is provided
    	if( is.null(.OctaveConfig) || !is.null(path) ){
            
            do_init <- TRUE
            # save Octave bin path
            if( !is.null(path) ) options(octave.path = path)
                        
            # create the config list at first call
    		conf <- list(bin =  octave_config('BINDIR')
                        , lib = octave_config(c('BINDIR', 'LIBDIR', 'OCTLIBDIR'))
    					, include = octave_config(c('INCLUDEDIR', 'OCTINCLUDEDIR'))
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
    		
            # save configuration
    		.OctaveConfig <<- conf
            
    	}
        
        # setup/initialise library and modules if necessary
        if( do_init ){
            if( isTRUE(.OctaveInit()) )
                .splash_message()
        }
        
        if( missing(name) ){
            if( !is.null(path) ) return( invisible(.OctaveConfig) )
            else return( .OctaveConfig )
        }
    		
    	settings <- .OctaveConfig[[name]]
        # settings may contain more than one path => sapply
    	unlist(sapply(settings, file.path, ...), use.names = FALSE)
	}
})

# Load/Unload Octave Libraries
#.OctaveLibs <- local({
#    .libs <- NULL
#    function(load = NULL){
#    		
#        if( is.null(load) ) return( .libs )
#    	            
#    	dyn.fun <- function(x, dlls){
#    		if( !x %in%  dlls ){
#                # add platform depend extension
#    			libname <- paste(x, .Platform$dynlib.ext, sep='')
#                
#                # On windows: Octave GCC dll files end with .a
#                if( .Platform$OS.type == 'windows' ){
#                    libname <- c(libname, paste(x, '-1', .Platform$dynlib.ext, sep = ''))
#                }
#                #
#                
#                # look-up and load dll files in the
#    			libs <- OctaveConfig('lib', libname)
#    			for( l in libs ){
#    				if( utils::file_test('-f', l) ){
#                        if( isTRUE(load) ) dyn.load(l) else dyn.unload(l)
#    					return( l )
#    				}
#    			}
#                # error if none of the expected path exist
#    			stop("Could not find Octave library '", x, "' in ", str_out(unique(dirname(libs)), Inf)
#                    , ".\n  File(s) looked up: ", str_out(basename(libs), Inf))
#    		}
#    	}
#    	
#        # load/unload required Octave libraries
#    	octlibs <- c('libcruft', 'liboctave', 'liboctinterp')
#        if( isFALSE(load) ) octlibs <- rev(octlibs)
#    	.libs <<- sapply(octlibs, dyn.fun, names(base::getLoadedDLLs()))    
#    }
#})

.OctaveInit <- local({
    .ncall <- 0L
    .initialised <- FALSE
    function(libname = NULL, pkgname = NULL){
    
        # do not call recursively
        if( .ncall > 0L || .initialised ) return()
        .ncall <<- .ncall + 1L
        on.exit( .ncall <<- .ncall - 1L )
        #
        
        # force libname and pkgname when called from OctaveConfig
        if( is.null(pkgname) ) pkgname <- packageName() 
        if( is.null(libname) ) libname <- dirname(path.package(pkgname))
        #
        
        # check Octave configuration is reachable
        if( is.null(octave_bindir <- octave_config('BINDIR', mustWork = FALSE)) ){
            return()
        }
        
    	# load required Octave libraries _before_ loading the package's library
    	#.OctaveLibs(TRUE)
        
        dlls <- base::getLoadedDLLs()
    	if ( !pkgname %in%  names(dlls) ){
        
            # setup path restoration for .onUnload
            on.exit( Sys.path$commit(), add = TRUE)
            Sys.path$append(octave_bindir)
                
            # load compiled library normally or in devmode
        	if( !isDevNamespace() ) library.dynam(pkgname, pkgname, libname)
        	else dyn.load(packagePath('src', paste0(pkgname, .Platform$dynlib.ext)))
        }
        
    	# start Octave session
    	octave_start()
        
    	# load Octave modules
    	octave_modules()
        
        # return TRUE
        .initialised <<- TRUE
    }
})

.onLoad <- function(libname, pkgname){
    
    # save initial PATH state to enable restoration in .onUnload
    Sys.path$init()
    
    # Initialise 
    .OctaveInit(libname, pkgname)
}

.onAttach <- function(libname, pkgname){
    
    .splash_message()
    
}

.splash_message <- function(){
    
    pversion <- utils::packageVersion('RcppOctave')
    if( is.null(octave_bindir <- octave_config('BINDIR', mustWork = FALSE)) ){
        packageStartupMessage("RcppOctave [", pversion, "] - Octave (not configured)"
                , "\nNOTE: Octave binaries were probably not found. See ?octave_config.")
    }else{
        # display info about config
        packageStartupMessage("RcppOctave [", pversion, "] - "
                , octave_version_string()
                , "\nOctave path: ", octave_bindir)
    }
}

.onUnload <- function(libpath) {
	
    # cleanup path on exit
    on.exit( Sys.path$revert("Reverting Octave changes to system PATH") )
    
    # terminate Octave session
    octave_end()
    
	# unload compiled library normally or in devmode
	dlls <- base::getLoadedDLLs()
	pname <- 'RcppOctave'
	if ( pname %in%  names(dlls) ){
		if( !missing(libpath) )	library.dynam.unload(pname, libpath)
		else dyn.unload(dlls[[pname]][['path']])
	}
	
	# unload required Octave libraries 
	#.OctaveLibs(FALSE)    
}


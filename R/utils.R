# Utility functions for RcppOctave
# 
# Author: Renaud Gaujoux
# Creation: 21 Nov 2011
###############################################################################

#' @import stringr
NULL

#' Compare Lists or Environments
#' 
#' This function compares two lists or environments.
#' It is useful for comparing results obtained in R and Octave.
#'  
#' @param x a \code{list} or an \code{environment}
#' @param y a \code{list} or an \code{environment}
#' @param msg a character string used (if not missing) in a message that is 
#' printed before the comparison. It is useful for separating multiple 
#' sequential comparisons.
#' 
#' @return No value is returned, but prints out:
#' \itemize{
#' \item the element/variable names of each input list or environment, 
#' \item the result of the comparison of the elements in \code{x} and the 
#' corresponding element in \code{y} -- if present.
#' }
#' 
#' 
#' @export
#' @examples
#' 
#' X <- matrix(1:64, 8)
#' ref <- svd(X)
#' res <- .O$svd(X, argout=3)
#' 
#' check.equal(ref, res, "R and Octave function 'svd'")
#' 
check.equal <- function(x, y, msg){
	
	l1 <- x; l2 <- y; 
	if( !missing(msg) ) message('check.equal: ', msg)
	if( is.environment(l1) ) l1 <- as.list(l1)
	if( is.environment(l2) ) l2 <- as.list(l2)
	
	cat("x: ", names(l1), "\n", sep=' ')
	cat("y: ", names(l2), "\n", sep=' ')
	na <- names(l1)[names(l1) %in% names(l2)]
	sapply(names(l1), function(n){
				cat(n, ':')
				ref <- l1[[n]]
				x <- l2[[n]]
				
				if( is.matrix(ref) && ncol(ref) == nrow(ref) && ncol(ref) == 1 && is.null(dim(x)) )
					ref <- drop(ref)
				if( is.numeric(ref) ){
					storage.mode(ref) <- 'double'
					if( is.matrix(x) && any(dim(x)==1) ){
						cat('*')
						x <- as.numeric(x)
					}
				}
				if( is.numeric(x) ){
					storage.mode(x) <- 'double'
					if( is.matrix(ref) && any(dim(ref)==1) ){
						cat('*')
						ref <- as.numeric(ref)
					}
				}
				
				if( is.null(x) ){
					if( is.null(ref) || all(dim(ref) == c(0,0)) )
						cat("OK0 | ")
					else cat("NULL | ")
				}
				else if( identical(ref, x) )
					cat("OK | ")
				else{
					e <- all.equal(ref, x)
					if( is.logical(e) && e )
						cat("OK2 (", sum(abs(ref-x)),")| ")
					else
						cat("ERROR | ")
				}
				
			})
	cat("\n")
	invisible()
}

packageName <- if( pkgmaker::testRversion("> 2.15.3") ) utils::packageName else pkgmaker:::packageName

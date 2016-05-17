/*
Header file for using internal C-level facilities
provided by RcppOctave.

Code adapted from file inst/include/xtsAPI.h in the xts package contributed by Jeffrey A. Ryan and Dirk Eddelbuettel
Copyright 2016 Renaud Gaujoux

This source is distributed with the same license
as the full RcppOctave software, GPL (>= 2).
*/

#ifndef _RCPP_OCTAVE_API_H
#define _RCPP_OCTAVE_API_H

#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include <Rconfig.h>
#include <R_ext/Rdynload.h>

#ifdef HAVE_VISIBILITY_ATTRIBUTE
  # define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
  # define attribute_hidden
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
  To aid those looking for answers on interfacing compiled code from
  another package.

  This is simply constructing a function pointer for use.

  static RETURNTYPE(*fun)(ARG1,ARG2) = NULL     where ARGS are the types accepted, comma separated
    fun = ( RETURNTYPE(*)(ARG1,ARG2)) R_GetCCallable("PACKAGENAME", "FUNCTIONNAME")

*/

/**
 * @param fname Octave function name
 * @param args Arguments passed as an R list object
 * @param argout the number of output values, or a vector of names to use as output
 * variable names. The names are directly used and applied to the result list in
 * their original order.
 *
 * The default value \code{argout=-1} returns:
 *
 *   - all output values when their number can be determined. This would typically
 * be the case for functions defined in .m files. Please do read section
 * \emph{Details} for considerations about the functions that use varargout.
 *   - only the first one otherwise.
 * }
 */
SEXP attribute_hidden CallOctave(Rcpp::CharacterVector fname, Rcpp::List args, SEXP argout = R_NilValue){

	using namespace Rcpp;

	static SEXP(*fun)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP) = NULL;
	if(fun == NULL)
		fun = (SEXP(*)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("RcppOctave","octave_feval");

	return fun(fname, args, argout, R_NilValue, R_NilValue, R_NilValue);
}


//SEXP attribute_hidden CallOctave(const char* fname, SEXP args, SEXP argout = R_NilValue){
//	return CallOctave(Rcpp::wrap(fname), args, argout);
//}


#ifdef __cplusplus
}
#endif

#endif /* !_RCPP_OCTAVE_API_H */

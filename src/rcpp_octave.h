#ifndef _RcppOctave_RCPP_OCTAVE_H
#define _RcppOctave_RCPP_OCTAVE_H

#include <RcppCommon.h>

// forward declaration of Octave classes
class octave_value;
class octave_value_list;
// declaring the specialization
namespace Rcpp {
	template <> SEXP wrap( const octave_value& );
	template <> SEXP wrap( const octave_value_list& );

	template <> octave_value as( SEXP );
	template <> octave_value_list as( SEXP );
}

// this must appear after the specialization,
// otherwise the specialization will not be seen by Rcpp types
#include <Rcpp.h>

// OCTAVE STUFF

// define version-specific macros
#include "compatibility.h"

// Octave libraries
#if !SWIG_OCTAVE_PREREQ(4,2,0) // version < 4.2.0
	#include <octave/config.h>
	#include <octave/oct-obj.h>
#else
	#include <octave/ovl.h>
#endif


#define VERBOSE_LOG if( RCPP_OCTAVE_VERBOSE ) Rprintf

#ifndef R_PACKAGE_NAME
#define R_PACKAGE_NAME "RcppOctave"
#endif

#define RcppOctave_error(funame, err) \
	{\
	std::ostringstream s;\
	s << R_PACKAGE_NAME << "::" << funame << " " << err;\
	throw std::string(s.str());\
	}

/**
 * Toggle verbosity for RcppOctave calls.
 *
 * @param value New value for verbosity (a boolean: true or false)
 *
 * @return the old value of verbosity
 */
RcppExport SEXP octave_verbose(SEXP value);

/*
 * Evaluate an Octave function
 *
 * @param fname function name as a character string.
 * @param args list of arguments that will be converted to native Octave types and
 * passed to the function.
 * @param output specifies the output values to extract: it can be a single integer giving the
 * number of output values, or a character vector that specifies the output names.
 * @param unlit logical that indicates if the output should be unlisted it consists in a single value
 * @param buffer logical that indicates if stdout and stderr should be buffered and
 * displayed at the end of the computation: 0 not buffered, 1 stdout buffered, 2, stderr buffered,
 * 3 both stdout and stderr buffered.
 * @param uuid unique call identifier
 *
 */
RcppExport SEXP octave_feval(SEXP fname, SEXP args, SEXP output, SEXP unlist, SEXP buffer, SEXP uuid);

/**
 * Start an Octave session from R.
 *
 * @param verbose Logical to toggle verbosity.
 */
RcppExport SEXP octave_start(SEXP verbose, SEXP with_warnings);

/**
 * Terminate an Octave session from R.
 */
RcppExport SEXP octave_end(SEXP verbose);

/**
 * Returns the help string from an Octave object.
 */
RcppExport SEXP oct_help(SEXP name);

// Register init/unload routines
RcppExport void R_init_RcppOctave(DllInfo *info);

RcppExport void R_unload_RcppOctave(DllInfo *info);

#endif

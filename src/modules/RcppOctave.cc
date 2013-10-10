// Octave module providing R callback feature for the R package RcppOctave
//
//	Copyright (C) 2011 Renaud Gaujoux
//
//	This file is part of RcppOctave.
//
//	RcppOctave is free software: you can redistribute it and/or modify
//	it under the terms of the GNU General Public License as published by
//	the Free Software Foundation, either version 3 of the License, or
//	(at your option) any later version.
//
//	RcppOctave is distributed in the hope that it will be useful,
//	but WITHOUT ANY WARRANTY; without even the implied warranty of
//	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//	GNU General Public License for more details.
//
//	You should have received a copy of the GNU General Public License
//	along with RcppOctave.  If not, see <http://www.gnu.org/licenses/>.

#include <octave/config.h>
#include <octave/oct-obj.h>
#include <octave/defun-dld.h>
#include <octave/ov-null-mat.h>
#include "../rcpp_octave.h"

// STD includes
#include <string>

extern bool RCPP_OCTAVE_VERBOSE;

// Prints an R object
template <typename T> void R_print(const T obj){
	using namespace Rcpp;
	BEGIN_RCPP
	RObject x = as<RObject>(wrap(obj));
	Environment base_env = Environment::base_env();
	Function R_fun = base_env["print"];
	R_fun(x);
	VOID_END_RCPP
}

/**
 * Simply returns its argument unchanged
 */
DEFUN_DLD (R_feval, args, nargout,
"USAGE: obj = R_feval(package, fun, ...)\n\n"
"Evaluate an R function.\n")
{
  // list of return values
  octave_value_list retval;

  // number of arguments supplied
  int nargs = args.length();

  // if wrong arguments, show message
  if (nargs < 1) {
    usage("R_feval - Wrong number of argument: expecting at least one arguments (the function's name)");
    // and return empty
    return retval;
  }

  using namespace Rcpp;
//  try{
	  const std::string s_fun(args(0).string_value());
	  // Create R argument list: skip package and function name
	  List all_args = as<List>(wrap(args));
	  List R_args(nargs - 1);
	  for(int i=1; i<nargs; ++i){
		  R_args(i-1) = all_args(i);
	  }

	  // call the R function using base::do.call
	  Environment base_env = Environment::base_env();
	  Function R_do_call = base_env["do.call"];
	  if( RCPP_OCTAVE_VERBOSE ){
		  Rprintf("Arguments:\n");
		  R_print(R_args);
	  }
	  // call R function
	  CharacterVector R_fun = as<CharacterVector>(wrap(s_fun));
	  RObject R_res = R_do_call(R_fun, R_args);
	  if( RCPP_OCTAVE_VERBOSE ){
		  Rprintf("Output:\n");
		  R_print(R_res);
	  }

	  // return result as an Octave value
	  retval(0) = as<octave_value>( wrap(R_res) );
	  return retval;

//	} catch( std::exception& __ex__ ){
//		usage( __ex__.what() ) ;
//	} catch(...){
//		usage( "c++ exception (unknown reason)" ) ;
//	}
}

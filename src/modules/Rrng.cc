// Octave modules for R Random Number Generator
// Copyright (C) 2011 Renaud Gaujoux <renaud@cbio.uct.ac.za>
//
// The code for the RNGs was extracted from R-2.14.0
// http://cran.r-project.org
//
// This code was initially inspired from Dirk Eddelbuettel's randmt Octave module.
// http://dirk.eddelbuettel.com/code/octave-mt.html
// Copyright (C) 1998, 1999 Dirk Eddelbuettel <edd@debian.org>
//
// This file provides the following Octave functions:
//    runif		for Uniform random number
//    rnorm		for Normal random number
//    rgamma	for Gamma random number
//    setseed	to set the seed of the current RNG
//
//     
//	This program is free software: you can redistribute it and/or modify
//	it under the terms of the GNU General Public License as published by
//	the Free Software Foundation, either version 3 of the License, or
//	(at your option) any later version.
//
//	This program is distributed in the hope that it will be useful,
//	but WITHOUT ANY WARRANTY; without even the implied warranty of
//	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//	GNU General Public License for more details.
//
//	You should have received a copy of the GNU General Public License
//	along with this program.  If not, see <http://www.gnu.org/licenses/>.

// Octave includes
#include <octave/config.h>
#include <octave/oct-obj.h>
#include <octave/oct-map.h>
#include <octave/defun-dld.h>

// STD includes
#include <limits.h>

#ifndef LIBRRNG_STANDALONE
//#include <RcppCommon.h>
//#include <Rdefines.h>
#include <Rmath.h>
#include <R_ext/Random.h>
#else
// Standalone libRrng
#include "libRrng.h"
#endif

#ifndef LIBRRNG_STANDALONE
#define INIT_LIBRRNG
#else
static void init(){
	static bool initRNG = true;
	if( initRNG ){
		//printf("Init RNG Seed\n");
		do_setseed(0, NULL, NULL);
		initRNG = false;
	}
}
#define INIT_LIBRRNG init();

DEFUN_DLD (setseed, args, nargout,
 "setseed(n)\n\n\
Sets the seed for the current RNG.\n")
{
  octave_value_list retval;	// list of return values
  int nargs = args.length ();	// number of arguments supplied

  if (nargs != 1) {		// if wrong arguments, show message
    usage("Wrong number of argument: expecting a single integer value.\nTry 'help setseed' for info");
    return retval;		// and return empty
  }

  long n = long(args(0).double_value());

  init();
  do_setseed((Int32) n, NULL, NULL);

  return retval;
}

//DEFUN_DLD (setRandomSeed, args, nargout,
// "setseed(n)\n\n\
//Sets the seed for the current RNG.\n")
//{
//  init();
//  octave_value_list retval;	// list of return values
//
//  int nargs = args.length ();	// number of arguments supplied
//
//  if (nargs != 1) {		// if wrong arguments, show message
//    usage("Wrong number of argument: expecting a single integer value.\nTry 'help setseed' for info");
//    return retval;		// and return empty
//  }
//
//  int n = args(0).length();
//  int* seed = args(0).int32_array_value();
//  set_Random_seed(seed, n);
//
//  return retval;
//}

DEFUN_DLD (getseed, args, nargout,
 "s = getseed()\n\n\
Returns the seed for the current RNG.\n\
The value correspond to the value of .Random.seed in R.\n\
See ?.Random.seed in an R session.")
{

  octave_value_list retval;	// list of return values
  int nargs = args.length ();	// number of arguments supplied

  if (nargs != 0) {		// if wrong arguments, show message
    usage("Wrong number of argument: expecting no arguments.\nTry 'help getseed' for info");
    return retval;		// and return empty
  }

  // initialize maximum
  int rs[MAX_SEED_LENGTH+1];
  int len = do_getseed(rs);
  int32NDArray m(dim_vector(1, len+1));
  for(int i=0; i<len+1; i++)
  	  m(0,i) = rs[i];
  retval(0) = m;

  return retval;
}

#endif

// Macros that define blocks for generic random generator Octave functions:
// * parameter check
// * value:
// rand(n) => n x n matrix
// rand(n, p) => n x p matrix
//
#define RAND_ARGS(octave_fun, min_arg, max_arg, iarg_row, iarg_col) \
INIT_LIBRRNG \
octave_value_list retval;\
int nargs = args.length ();	\
\
if (nargs<min_arg || nargs>max_arg) { \
std::stringstream err;\
err << "Invalid call to '" << octave_fun << "': ";\
if( nargs<min_arg ) err << "at least " << min_arg << " argument(s) required.";\
else err << "too many arguments.";\
err << std::endl << "See 'help " << octave_fun << "' for more details.";\
usage(err.str().c_str());\
return retval;\
}\
\
long n,k;\
if( nargs <= iarg_row ){\
	n = k = 1;\
}else{\
	octave_value tmp = args(iarg_row);\
	if( tmp.is_matrix_type() ){\
		Array<int> iv = tmp.int_vector_value (true);\
		n = iv(0);\
		k = iv(1);\
	}else{\
		n = long(args(iarg_row).double_value());\
		k = ( nargs > iarg_col && !args(iarg_col).is_empty() ? long(args(iarg_col).double_value()) : n);\
	}\
}\
Matrix X(n, k);

#define RAND_RESULT(fun_rand_call) \
GetRNGstate(); /* update R internal random seed*/\
for (long j=0; j<k; j++){\
	for (long i=0; i<n; i++){\
	  X(i,j) = fun_rand_call;\
	}\
}\
PutRNGstate(); /* update R variable .Random.seed */\
retval(0) = X;\
\
return retval;

#define RAND_FUNCTION(fun_rand, octave_fun) \
RAND_ARGS(octave_fun, 0, 2, 0, 1) \
RAND_RESULT(fun_rand())

#define RCPP_OCTAVE_HELP_NOTE \
"\n\nNOTE:\n"\
"This function substitutes Octave original function in calls from RcppOctave\n\n"

DEFUN_DLD (rand, args, nargout,
"USAGE: U = rand( n [, k])\n\n"
"Generates uniform random variates as R function 'runif' -- using the current RNG from R."
"\n\nPossible calls:\n"
"rand(n, k)   returns a n*k matrix with uncorrelated U(0, 1) deviates drawn in columns\n"
"rand(n)      returns a n*n matrix with uncorrelated U(0, 1) deviates drawn in columns\n"
RCPP_OCTAVE_HELP_NOTE)
{
	 RAND_FUNCTION(unif_rand, "rand")
}

DEFUN_DLD (randn, args, nargout,
 "USAGE: N = randn( n [, k])\n\n"
"Generates standard-normal random variates as R function 'rnorm' -- using the current RNG from R."
"\n\nPossible calls:\n"
"randn(n, k)   returns a n*k matrix with uncorrelated N(0, 1) deviates drawn in columns\n"
"randn(n)      returns a n*n matrix with uncorrelated N(0, 1) deviates draw in columns\n"
RCPP_OCTAVE_HELP_NOTE)
{
	RAND_FUNCTION(norm_rand, "randn")
}

DEFUN_DLD (rande, args, nargout,
 "USAGE: E = rande( n [, k])\n\n"
"Generates standard-exponential random variatesas R function 'rexp' -- using the current RNG from R."
"\n\nPossible calls:\n"
"rande(n, k)   returns n*k matrix with uncorrelated E(0, 1) deviates drawn in columns\n"
"rande(n)      returns n*n matrix with uncorrelated E(0, 1) deviates drawn in columns\n"
RCPP_OCTAVE_HELP_NOTE)
{
	RAND_FUNCTION(exp_rand, "rande")
}

DEFUN_DLD (randg, args, ,
"USAGE: E = randg(shape, [n, p, scale])\n\n"
"Generates Gamma random variates as R function 'rgamma' -- using the current RNG from R."
"\n\nPossible calls:\n"
"randg(shape)				returns a single draw from G(shape, 1)\n"
"randg(shape, n) 			returns n*n matrix with uncorrelated G(shape, 1) deviates drawn in columns\n"
"randg(shape, n, p) 		returns n*p matrix with uncorrelated G(shape, 1) deviates drawn in columns\n"
"randg(shape, n, p, scale) 	returns n*p matrix with uncorrelated G(shape, scale) deviates drawn in columns\n"
RCPP_OCTAVE_HELP_NOTE)
{
	
  RAND_ARGS("randg", 1, 4, 1, 2)

  int nArgs = args.length ();	// number of arguments supplied

  // retrieve shape and scale parameters
  double shape = args(0).double_value();
  double scale(nArgs >= 4 && !args(3).is_empty() ? args(3).double_value() : 1);

  RAND_RESULT(rgamma(shape, scale))\
}


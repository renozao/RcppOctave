/* Copyright (c) 1996-2009 by John W. Eaton.
   Copyright (c) 2003 by Paul Kienzle.
   Copyright (c) 2010 by Albert Graef.
   Copyright (c) 2011 by Renaud Gaujoux.

   This file is part of the R package RcppOctave.

   RcppOctave is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   RcppOctave is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.

   Please see the accompanying COPYING file for the precise license terms. The
   GPL are also be read online at http://www.gnu.org/licenses/.
*/

#ifdef _WIN64

#include <Rdefines.h>

extern "C" {

	/** Dummy interface function */
	SEXP octave_feval(SEXP fname, SEXP args, SEXP output, SEXP unlist, SEXP buffer){
		return R_NilValue;
	}
}

#else

#include "rcpp_octave.h"
#include "Redirect.hpp"

// R includes
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Octave includes
#include <octave/oct.h>
#include <octave/octave.h>
#include <octave/input.h>

#include <octave/pt-all.h>
#if SWIG_OCTAVE_PREREQ(4,3,0) // version >= 4.3.0
#include <octave/str-vec.h>
#endif
#include <octave/symtab.h>
#include <octave/parse.h>
//#if OCTAVE_API_VERSION_NUMBER < 45
#if !SWIG_OCTAVE_PREREQ(3,4,0)
#include <octave/unwind-prot.h>
#endif
#if !SWIG_OCTAVE_PREREQ(4,2,0) // version < 4.2.0
#include <octave/toplev.h>
#else
#include <octave/interpreter.h>
#endif
#include <octave/error.h>
#include <octave/quit.h>
#include <getopt.h>

#if SWIG_OCTAVE_PREREQ(4,1,0) // version >= 4.1.0
// Must handles issue #14 here
// see: http://octave.org/doxygen/4.1/da/d0d/signal-wrappers_8h.html
#include "signal-wrappers.h"
#endif
#include <octave/variables.h>
#include <octave/sighandlers.h>
#include <octave/sysdep.h>

#include <ov-usr-fcn.h>

// STD includes
#include <iostream>
#include <string>
using namespace std;

static bool OCTAVE_INITIALIZED = false;

/**
 * Global variable to hold verbosity status.
 */
bool RCPP_OCTAVE_VERBOSE = false;
static std::string _UUID("");

/* Octave embedded interpreter.
 * This is a mix between embedded versions of Octave from:
 * 1) pure-octave by Albert Graef:
 * (source: http://docs.pure-lang.googlecode.com/hg/pure-octave.html) 
 * It is described as a "heavily hacked version of octave_embed by 
 * Paul Kienzle (http://wiki.octave.org/wiki.pl?OctaveEmbedded) which 
 * in turn is based on Octave's toplevel".
 *
 * 2) SHOGUN
 * (source: http://shogun-toolbox.org/trac/browser/src/octave/)
 */
octave_value octave_feval(const string& fname, const octave_value_list& args, int nres=-1
						, const std::vector<string>* output_names=NULL
						, int buffer = 3);
inline octave_value octave_feval(const string& fname, const octave_value_list& args
								, const std::vector<string>& output_names
								, int buffer = 3){
		return octave_feval(fname, args, output_names.size(), &output_names, buffer);
}

/**
 * Toggle verbosity for RcppOctave calls.
 */
SEXP octave_verbose(SEXP value){

	bool res = RCPP_OCTAVE_VERBOSE;
	// set verbosity if not passing NULL
	if( !Rf_isNull(value) ){
		bool bval = Rcpp::as<bool>(value);
		RCPP_OCTAVE_VERBOSE = bval;
	}
	return( Rcpp::wrap(res) );
}

#if SWIG_OCTAVE_PREREQ(4,3,0)
typedef octave::tree_parameter_list tree_parameter_list;
#endif

bool octave_session(bool start=true, bool with_warnings = true, bool verbose = false){

	// use global verbose state if set
	bool R_RCPPOCTAVE_DEBUG = getenv("R_RCPPOCTAVE_DEBUG") != NULL;
	with_warnings = R_RCPPOCTAVE_DEBUG || RCPP_OCTAVE_VERBOSE || with_warnings;
	verbose = R_RCPPOCTAVE_DEBUG || RCPP_OCTAVE_VERBOSE || verbose;
#if SWIG_OCTAVE_PREREQ(4,2,0)
	static octave::embedded_application *the_app = NULL;
	static octave::interpreter *embedded_interpreter = NULL; //(&the_app, true);
#endif

	if( start ){
		if( verbose ) REprintf("Starting Octave interpreter ... ");

		if( OCTAVE_INITIALIZED ){// early exit if alredy on
			if( verbose ) REprintf("[SKIP: already on]\n");
			return true;
		}

		// instantiate the Octave interpreter
		int narg = 4;
		int narg_overflow = optind + 1; // workaround for bug in Octave 4.2 and later:
		// octave.cc (v4.2, 4.3+) cmdline_options::cmdline_options() uses
		// argv + octave_optind_wrapper (), where argv is the char** produced below
		// and optind is the number of arguments getopt.h reports for the executable,
		// having nothing to do with the args produced here. If the executable
		// was called with more than narg arguments, this gives a SEGFAULT.
		string_vector cmd_args(narg + narg_overflow);
		cmd_args(0) = std::string("RcppOctave");
		cmd_args(1) = std::string("--quiet");
		// Try avoid corruption of R console with Octave console outputs
		// [suggested by Albert Graef]
		cmd_args(2) = std::string("--no-line-editing");
		cmd_args(3) = std::string("--no-history");
		//static octave::cmdline_options opts(narg, cmd_args.c_str_vec());
		octave::cmdline_options opts(narg, cmd_args.c_str_vec());

		// redirect both stderr and stdout
		Redirect redirect(7);

		// try starting Octave
#if SWIG_OCTAVE_PREREQ(4,2,0)
		the_app = new octave::embedded_application(opts);
		int return_code = the_app->execute();
#else
		int return_code = octave_main(narg, cmd_args.c_str_vec(), true /*embedded*/);
#endif
		if( verbose ) REprintf(!return_code ? "[OK]\n" : "[ERROR]\n");
		int warn = (with_warnings ? 1 : 0) * (verbose ? 2 : 1);
		redirect.flush("Failed to start Octave interpreter", !return_code, warn);

		OCTAVE_INITIALIZED = true;
#if !SWIG_OCTAVE_PREREQ(3,8,0)
		bind_internal_variable("crash_dumps_octave_core", false);
#endif

	}else{
		if( verbose )
			REprintf("Terminating Octave interpreter... ");

		if( !OCTAVE_INITIALIZED ){// early exit if already off
			if( verbose ) REprintf("[SKIP: already off]\n");
			return true;
		}

		// terminate interpreter
#if SWIG_OCTAVE_PREREQ(3,8,0)
#if !SWIG_OCTAVE_PREREQ(4,2,0)
		octave_exit = 0;
#endif
		try {
			clean_up_and_exit(0, true);
		} catch (const octave::exit_exception& ex) {
			if(ex.exit_status() != 0) {
				std::ostringstream err;
				err << R_PACKAGE_NAME" - error exiting Octave.";
				throw std::string(err.str());
			}
		}
#else
		do_octave_atexit();
#endif
#if SWIG_OCTAVE_PREREQ(4,2,0)
		if( the_app ) {
#if !SWIG_OCTAVE_PREREQ(4,3,0)
			// workaround for Bus Error upon exit in 4.2
			symbol_table::clear_all(true);
#endif
			delete the_app;
			the_app = NULL;
		}
#endif

		if( verbose )
			REprintf("[OK]\n");
		OCTAVE_INITIALIZED = false;
	}

	return true;
}

SEXP octave_start(SEXP verbose, SEXP with_warnings){

	if( !Rf_isNull(verbose) )
		RCPP_OCTAVE_VERBOSE = Rcpp::as<bool>(verbose);
	bool _warnings = Rcpp::as<bool>(with_warnings);
	return Rcpp::wrap(octave_session(true, _warnings));
}

SEXP octave_end(SEXP verbose = R_NilValue){

	bool b_verbose = !Rf_isNull(verbose) ? Rcpp::as<bool>(verbose) : false;
	return Rcpp::wrap(octave_session(false, true, b_verbose));
}

void R_init_RcppOctave(DllInfo *info)
{
	/* Register routines, allocate resources. */

	/* used by external packages linking to internal xts code from C */
	R_RegisterCCallable(R_PACKAGE_NAME, "octave_feval",(DL_FUNC) (SEXP(*)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP))&octave_feval);

	// set verbosity from environment variable
	RCPP_OCTAVE_VERBOSE = getenv("R_RCPPOCTAVE_VERBOSE") != NULL;
	// start octave session
	octave_session(true, false);
}

void R_unload_RcppOctave(DllInfo *info)
{
	/* Release resources. */
	octave_end();
}

/**
 * Recover from an exception.
 * It restores some Octave static variables into a no-error state.
 *
 * @note OCTAVE_API_VERSION_NUMBER is 47 for 3.4.0 but 45 for 3.4.2
 * see: http://octave.1599824.n4.nabble.com/API-version-going-backwards-td3722496.html
 */
extern void recover_from_exception_rcppoct(void)
{
//#if OCTAVE_API_VERSION_NUMBER >= 45
#if SWIG_OCTAVE_PREREQ(3,4,0)
#else
  // This isn't supported in the latest Octave versions. We simply leave this
  // disabled for now, which means that you'll have to use 'unwind_protect'
  // explicitly in your Octave code in order to handle Octave exceptions.
  // XXXFIXME: This might leak memory in some cases??
  unwind_protect::run_all ();
#endif

#if !SWIG_OCTAVE_PREREQ(4,2,0) // version < 4.2.0
  can_interrupt = true;
#else
  octave::can_interrupt = true;
#endif
  octave_interrupt_immediately = 0;
  octave_interrupt_state = 0;
  octave_signal_caught = 0;
  octave_exception_state = octave_no_exception;
  // prior to 3.2.0
  // octave_allocation_error = 0;
  octave_restore_signal_mask ();
#if !SWIG_OCTAVE_PREREQ(4,2,0) // version < 4.2.0
  octave_catch_interrupts ();
#else
    octave::catch_interrupts ();
#endif
}

typedef std::vector<string> std_vector;
SEXP octave_feval(SEXP fname, SEXP args, SEXP output = R_NilValue, SEXP unlist = R_NilValue, SEXP buffer = R_NilValue, SEXP uuid = R_NilValue){

	using namespace Rcpp;
	BEGIN_RCPP

	// unlist result?
	bool do_unlist = Rf_isNull(unlist) ? true : as<bool>(unlist);
	// buffer stdout/stderr?
	int buffer_std = Rf_isNull(buffer) ? -1 : as<int>(buffer);
	// update static UUID if necessary
	if( !Rf_isNull(uuid) ){
		string newUUID = Rcpp::as<string>(uuid);
		if( _UUID != newUUID ){
			_UUID = Rcpp::as<string>(uuid);
			octave_feval("octave_uuid", Rcpp::as<octave_value>(uuid));
		}
	}
	VERBOSE_LOG("octave_feval - UUID: %s\n", _UUID.c_str());

	SEXP argout = Rf_isNull(output) ? wrap<int>(1) : output;
	octave_value out;
	if( TYPEOF(argout) == STRSXP ){
		out = octave_feval(Rcpp::as<string>(fname)
						, Rcpp::as<octave_value_list>(args)
						, Rcpp::as<std_vector>(argout)
						, buffer_std);
	}else{
		out = octave_feval(Rcpp::as<string>(fname)
						, Rcpp::as<octave_value_list>(args)
						, Rcpp::as<int>(argout)
						, NULL
						, buffer_std);
	}

	// special case of no result
	if( out.is_empty() ){
		return R_NilValue;
	}

	// unlist result if requested
	if( do_unlist ){
		if( out.is_cs_list() PRE_3_4_0(|| out.is_list()) ){ // unnamed list

			octave_value_list ol = out.list_value();
			if ( ol.length() == 1){
				VERBOSE_LOG("octave_feval - Unlisting unnamed output\n");
				return Rcpp::wrap(ol(0));
			}

		}else if( out.is_map() ){ // named list

			OCTAVE_MAP m = out.map_value();
			if (m.nfields() == 1){
				const string_vector& keys = m.keys();
				VERBOSE_LOG("octave_feval - Unlisting named output '%s'\n", keys[0].c_str());
				return Rcpp::wrap(m.contents(keys[0])(0));
			}

		}else{
			// throw an R error
			std::ostringstream err;
			err << R_PACKAGE_NAME" - could not unlist the result of function `"
					<< Rcpp::as<string>(fname).c_str()
					<< "`: unsupported returned value type [" << out.type_name().c_str() << "].";
			throw std::string(err.str());
		}
	}

	return Rcpp::wrap(out);

	} catch ( const std::string& s){ // catch octave errors
		Rf_error("%s", s.c_str());
	END_RCPP
}

/**
 * Extract output names from an Octave function.
 *
 * This works only for functions defined in m-files, and in Octave >= 3.4.0.
 */
int getOutnames(const string& fname, std::vector<string>& onames){

#if OCT_POST_3_4_0 < 0
	return -1;
#else
	// Lookup this name in the symbol tables
	octave_value fdef = symbol_table::find(fname);
	onames.clear();

	VERBOSE_LOG("octave_feval - Check if `%s` has an internal symbol table ... ", fname.c_str());
	if( !fdef.is_user_function() ){
		VERBOSE_LOG("NO\n");
		return -1;
	}
	VERBOSE_LOG("YES\n");

	VERBOSE_LOG("octave_feval - Check if output names are detectable ... ");
	octave_user_function *f = (octave_user_function*)fdef.function_value();
	if( f == NULL ){
		VERBOSE_LOG("NO\n");
		return -1;
	}
	tree_parameter_list *rl = f->return_list ();
	if( rl == NULL ){
		VERBOSE_LOG("NO\n");
		return -1;
	}

	int nres = rl->length();
	VERBOSE_LOG("YES [%i]\n", nres);

	// limit number of output variables
	if( nres == 0 ) return 0;

	VERBOSE_LOG("octave_feval - Output name(s):");
	onames.reserve(nres);
	tree_parameter_list::iterator rlp = rl->begin();
	for(int i=0; rlp != rl->end(); rlp++)
	{
		tree_identifier *rid = (*rlp)->ident();
		if (rid)
		{
			// retrieve the variable names
			onames.push_back(rid->name());
			VERBOSE_LOG(" '%s'", onames[i].c_str());
			++i;
		}
	}
	VERBOSE_LOG("\n");

	return nres;
#endif
}


octave_value octave_feval(const string& fname, const octave_value_list& args, int nres
						, const std::vector<string>* output_names
						, int buffer) {

	VERBOSE_LOG("octave_feval - Calling Octave function `%s` with %i argument(s)\n", fname.c_str(), args.length());
	VERBOSE_LOG("octave_feval - Buffering level: %i\n", buffer);

	// update time stamps to reload modified functions if necessary
	Vlast_prompt_time.stamp();

	octave_save_signal_mask();
	if (octave_set_current_context) {
#if defined (USE_EXCEPTIONS_FOR_INTERRUPTS)
		panic_impossible()
#else
	//#if OCTAVE_API_VERSION_NUMBER >= 45
	#if SWIG_OCTAVE_PREREQ(3,4,0)
	#else
		//XXX FIXME XXX
		unwind_protect::run_all ();
	#endif
		raw_mode(0);
		Rprintf("\n");
		octave_restore_signal_mask();
#endif
	}

#if !SWIG_OCTAVE_PREREQ(4,2,0) // version < 4.2.0
	can_interrupt = true;
	octave_catch_interrupts();
#else
	octave::can_interrupt = true;
	octave::catch_interrupts();
#endif
	octave_initialized = true;

	// setup catching of stderr to use R stderr own functions
	Redirect redirect(buffer, true);// delay until calling redirect
	//

	try {

		reset_error_handler();

		// extract the output names
		std::vector<string> autonames;
		if( output_names != NULL )
			nres = output_names->size();
		else{
			int n_output = getOutnames(fname, autonames);
			// set default (max) number of result if necessary and possible
			VERBOSE_LOG("octave_feval - Requested number of return value(s): ");
			if( nres < 0 ){
				nres = n_output == -1 ? 1 : n_output;
				VERBOSE_LOG("%i [default]\n", nres);
			}else if( n_output > nres ){ // the user requested less than the maximum output
				VERBOSE_LOG("%i [truncate]\n", nres);
				autonames.clear();
			}else if( n_output == -1 ){
					VERBOSE_LOG("%i [force]\n", nres);
					autonames.clear();
			}else{
				VERBOSE_LOG("%i\n", nres);
			}
			output_names = &autonames;
		}
		const std::vector<string>& onames = *output_names;

		VERBOSE_LOG("octave_feval - Calling feval now ... ");
		// catch stderr if requested
		redirect.redirect();
		octave_value_list out = feval(fname, args, nres);
		if ( !error_state ){

			redirect.flush();

			VERBOSE_LOG("OK\noctave_feval - Result has %i elements\n", out.length());

			// reduce the number of result elements if necessary
			if( nres < out.length() ){
				VERBOSE_LOG("octave_feval - Limit result to %i elements\n", nres);
				out = out.slice(0, nres);
			}

			// directly return the result if no output names are available
			if( onames.size() == 0 ){
				return out;
			}else{ // return the result as a map
				int n = onames.size();
				if( n != out.length() ){
					warning("Dropping names due to inconsistent lengths");
					return out;
				}

				VERBOSE_LOG("octave_feval - Set output name(s):");
				// add output names
				OCTAVE_MAP m(dim_vector(n, 1));
				for (int i=0; i<n; ++i){
					const string& s = onames[i];
					VERBOSE_LOG(" '%s'", s.c_str());
					if( s[0] == '\0' ){ // error
						RcppOctave_error(fname, "error in parsing result: empty output name.");
					}
					m.assign(s, out(i));
				}
				VERBOSE_LOG("\n");
				return octave_value(m);
			}
		} else {
			VERBOSE_LOG("ERROR\n");
			VERBOSE_LOG(R_PACKAGE_NAME" - error in Octave function `%s`.\n", fname.c_str());
		}
	} catch	(
#if !SWIG_OCTAVE_PREREQ(4,2,0) // version < 4.2.0
			octave_interrupt_exception
#else
			octave::interrupt_exception
#endif
	){
		REprintf(R_PACKAGE_NAME" - Caught Octave exception: interrupt\n");
		recover_from_exception_rcppoct();
		REprintf("\n");
		//error_state = -2;
	}
	catch (std::bad_alloc)
	{
		REprintf(R_PACKAGE_NAME" - Caught Octave exception: bad_alloc\n");
		recover_from_exception_rcppoct();
		REprintf("\n");
		//error_state = -3;
	} catch(const octave::execution_exception& e) {
			std::ostringstream err;
			err << R_PACKAGE_NAME" - Octave error: execution_exception";
			if(!e.info().empty()) err << "(" << e.info() << ")";
			recover_from_exception_rcppoct();
			throw std::string(err.str());
	}
	octave_restore_signal_mask();
	octave_initialized = false;

	// throw an R error
	std::ostringstream err;
	err << R_PACKAGE_NAME" - error in Octave function `" << fname.c_str() << "`";
	redirect.flush(err.str().c_str(), true);

	return octave_value_list();
}

/** Returns the help string from an Octave object. */
SEXP oct_help(SEXP name){
	using namespace Rcpp;

	BEGIN_RCPP
	// load name into a list
	List args(1);
	args[0] = name;

#if OCT_POST_3_4_0 < 0 // prior to 3.4.0: help directly prints out the documentation
	// redirect std::out
	Redirect redirect(1);
	octave_value ores = octave_feval(wrap("help"), wrap(args), wrap(0));
	return( wrap(redirect._cout.str()) );
#else
	return( octave_feval(wrap("help"), wrap(args), wrap(1)) );
#endif

	END_RCPP
}

#if 0
	int main(void){

		octave_feval(R_NilValue, R_NilValue, R_NilValue);

		octave_end();

		return 0;
	}
#endif

#endif // END not _WIN64

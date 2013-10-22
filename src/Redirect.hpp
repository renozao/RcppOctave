#ifndef REDIRECT_H
#define REDIRECT_H

#include <iostream>
#include <string>
#define R_NO_REMAP
#include <R.h>
/**
 * Output redirection utility class
 */
class Redirect{

public:

//	struct nullstream : ofstream {
//		nullstream() : ofstream( MSWIN_ALT("/dev/null", "NUL") ) { }
//	};

private:

	/** NULL stream to sink output */
//	nullstream _nulldev;

	/** backup stream of standard cout to restore stream when finished */
	std::streambuf* _old_cout_buf;
	std::streambuf* _old_cerr_buf;
	int _stdType;
public:
	std::stringstream _cout;
	std::stringstream _cerr;

public:

	void redirect(int type = 3){
		// save output/err buffer of the stream and redirect
		if( type < 0 ) type = 3 + type;
		_stdType = type;
		if( type & 1 ){
			_old_cout_buf = std::cout.rdbuf();
			std::cout.rdbuf(_cout.rdbuf());

		}
		if( type & 2 ){
			_old_cerr_buf = std::cerr.rdbuf();
			std::cerr.rdbuf(_cerr.rdbuf());

		}
	}

	Redirect() : _old_cout_buf(NULL), _old_cerr_buf(NULL), _stdType(0){
	}

	Redirect(int type) : _old_cout_buf(NULL), _old_cerr_buf(NULL), _stdType(type){
		// save output/err buffer of the stream and redirect
		redirect(_stdType);
	}

	void flush(const char* omsg = NULL, const char* emsg = NULL, const char* wmsg = NULL
				, bool stop = false, bool warn = true){

		// Output
		const std::string stdout_str = _cout.str();
		if( stdout_str.length() > 0 ){
			std::ostringstream out;
			if( omsg != NULL ) out << omsg << ":" << std::endl << "  ";
			out << stdout_str;
			Rprintf("%s", out.str().c_str());
			_cout.clear();
		}

		// Error/Warning
		const std::string stderr_str = _cerr.str();
		if( stop || stderr_str.length() > 0 ){
			std::ostringstream err;
			const char* msg = stop ? emsg : wmsg;
			if( msg != NULL ) err << msg << ":" << std::endl << "  ";
			err << stderr_str;
			_cerr.clear();
			// throw an exception not Rf_error
			// See: http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2010-May/000651.html
			if( stop ) throw std::string(err.str());
			else if( warn ) Rf_warning("%s", err.str().c_str());
		}
	}

	void end(){
		// restore old output buffer
		if( _stdType & 1 ) std::cout.rdbuf(_old_cout_buf);
		if( _stdType & 2 ) std::cerr.rdbuf(_old_cerr_buf);

		_stdType = 0;
		// clear streams and pointers
		_old_cout_buf = _old_cerr_buf = NULL;
		_cout.clear(); _cerr.clear();
	}

	virtual ~Redirect(){
		end();
	}
};

#endif

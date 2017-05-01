#ifndef REDIRECT_H
#define REDIRECT_H

#include <cstdio>
#include <streambuf>
#include <iostream>
#include <string>
#include <Rcpp.h>
#include "octave/pager.h"

template <bool OUTPUT>
class Octave_Rstreambuf : public Rcpp::Rstreambuf<OUTPUT> {

		int _sink;
		std::stringstream _output;
		std::stringstream _errors;
		std::stringstream _warnings;
		std::stringstream _usages;
	public:
		Octave_Rstreambuf(int sink_level = 0) :
			Rcpp::Rstreambuf<OUTPUT>()
			, _sink(sink_level)
		{
			// clear all buffers
			_output.str(std::string());
			_errors.str(std::string());
			_warnings.str(std::string());
			_usages.str(std::string());
		}

		void send_to_R(const char* head = NULL, bool stop = false, int warn = 1);

		std::string str() const{
			return _output.str();
		}

	protected:
		virtual std::streamsize xsputn(const char *s, std::streamsize n );

		// dump usage strings as errors or normal output
		void dump_usage(bool as_error = false){
			if( _usages.str().length() ){
				if( as_error ) _errors << _usages.str() << "  ";
				else _output << _usages.str();
				// clear usage buffer
				_usages.str(std::string());
			}
		}
};

template <> inline void Octave_Rstreambuf<true>::send_to_R(const char* head, bool stop, int warn){

	// Output: write to R stdout
	std::string buf_msg = _output.str();
	if( head != NULL ) Rcpp::Rcout << head << ":" << std::endl << "  ";
	if( buf_msg.length() > 0 ){
		Rcpp::Rcout << _output.str();
		// clear buffer
		_output.str(std::string());
	}
}

template <> inline void Octave_Rstreambuf<false>::send_to_R(const char* head, bool stop, int warn){

	// flush usage: in errors if in stop mode
	dump_usage(stop);

	// Messages: write to R stderr
	if( _output.str().length() > 0 ){
		REprintf(_output.str().c_str());
		_output.str(std::string());
	}

	// Warnings: throw R warnings
	std::string buf_msg = _warnings.str();
	if( warn && buf_msg.length() > 0 ){
		std::ostringstream omsg;
		if( head != NULL ) omsg << head << ":" << std::endl << "  ";
		omsg << buf_msg;
		_warnings.str(std::string());
		Rf_warning("%s", omsg.str().c_str());
	}

	// Errors
	buf_msg = _errors.str();
	if( stop || buf_msg.length() > 0 ){
		std::ostringstream omsg;
		if( head != NULL ) omsg << head << ":" << std::endl << "  ";
		omsg << buf_msg;
		_errors.str(std::string());
		// throw an exception not Rf_error
		// See: http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2010-May/000651.html
		if( warn == 2 ) REprintf("%s\n", omsg.str().c_str());
		throw std::string(omsg.str());
	}

}

template <> inline std::streamsize Octave_Rstreambuf<true>::xsputn(const char *s, std::streamsize n ){

	// sink std output
	if( _sink & 1 ){
		_output << s;
		return(n);
	}
	// send to R
	return Rcpp::Rstreambuf<true>::xsputn(s, n);
}

template <> inline std::streamsize Octave_Rstreambuf<false>::xsputn(const char *s, std::streamsize n ){

	//detect warning/error
	if( strstr(s, "error: ") == s || error_state ){
		// flush possible previous usage messages
		dump_usage(true);

		_errors << (s + 7);
		if( _sink & 2 ) return(n); // sink errors

	}else if( strstr(s, "usage: ") == s ){ // usage string
		_usages << s;

		if( _sink & 2 ) return(n); // sink errors

	}else if( strstr(s, "warning: ") == s ){
		// flush possible previous usage messages
		dump_usage();
		// sink warnings? (store to throw them later)
		if( _sink & 4 ) _warnings << (s+9);
		else Rf_warning("%s", s+9);

		// never output plain warning
		if( _sink != 0 ) return(n);

	}else if( _sink & 2 ){ // sink messages
		// flush possible previous usage messages
		dump_usage();
		_output << s;
		return(n);
	}
	// Send to R
	return Rcpp::Rstreambuf<false>::xsputn(s, n);
}

template <bool OUTPUT>
class Octave_Rostream : public std::ostream {
	typedef Octave_Rstreambuf<OUTPUT> Buffer ;
	Buffer* buf;
public:
	Octave_Rostream(int sink_level = 0):
		std::ostream( new Buffer(sink_level) ),
		buf(static_cast<Buffer*>( rdbuf() ) )
	{}

	~Octave_Rostream(){
		if (buf != NULL) {
			delete buf;
			buf = NULL;
		}
	}

	Buffer* Rrdbuf(){
		return static_cast<Buffer*>( rdbuf() );
	}

	std::string str() const{
		return buf->str();
	}
};


/**
 * Output redirection utility class
 */
class Redirect{

private:

	/** NULL stream to sink output */
//	nullstream _nulldev;

	/** backup stream of standard cout to restore stream when finished */
	std::streambuf* _old_cout_buf;
	std::streambuf* _old_cerr_buf;
	int _stdType;
public:
	Octave_Rostream<true> _cout;
	Octave_Rostream<false> _cerr;

public:

	void redirect(){

		if( !_stdType ) return;

		// save output/err buffer of the stream and redirect
		_old_cout_buf = octave_stdout.rdbuf();
		octave_stdout.rdbuf( _cout.rdbuf() );
		_old_cerr_buf = std::cerr.rdbuf();
		std::cerr.rdbuf( _cerr.rdbuf() );
	}

	Redirect() : _old_cout_buf(NULL), _old_cerr_buf(NULL), _stdType(0){
	}

	Redirect(int type, bool delay = false) :
		_old_cout_buf(NULL), _old_cerr_buf(NULL)
		, _stdType(type < 0 ? 7 + type : type)
		, _cout(type < 0 ? 7 + type : type)
		, _cerr(type < 0 ? 7 + type : type){
		// save output/err buffer of the stream and redirect
		if( !delay ) redirect();

	}

	void flush(const char* head = NULL, bool stop = false, int warn = 1){
		// stdout
		_cout.Rrdbuf()->send_to_R();
		// stderr
		_cerr.Rrdbuf()->send_to_R(head, stop, warn);

	}

	void end(){

		if( !_stdType ) return;
		// restore old output buffer
		octave_stdout.rdbuf(_old_cout_buf);
		std::cerr.rdbuf(_old_cerr_buf);

		// clear pointers to backup streams
		_old_cout_buf = _old_cerr_buf = NULL;
	}

	virtual ~Redirect(){
		end();
	}
};

#endif

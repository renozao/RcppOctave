#include "rcpp_octave.h"

#define R_NO_REMAP
#include <Rdefines.h>
#define getAttrib Rf_getAttrib

#include <octave/ov-base.h>
#include <octave/ov-scalar.h>
#include <octave/ov-struct.h>
#include <octave/ov-null-mat.h>

#include <string.h>
using namespace std;

extern bool RCPP_OCTAVE_VERBOSE;

#define DEBUG_OCT_VALUE(val) \
	Rprintf("Integer value - int8: %i | int16: %i | int32: %i | int64: %i | int: %i\n", \
		val.is_int8_type(), val.is_int16_type() \
		, val.is_int32_type(), val.is_int64_type() \
		, val.is_integer_type());

#define WRAP_ERROR(err) RcppOctave_error("wrap", err);

/**
* Converts an Octave Array into an R matrix or vector.
*/
template <int RTYPE, typename T> SEXP wrapArray(const Array<T>& x){

	// compute dimensions
	int n = x.rows();
	int p = x.cols();
	int L = n*p;

	VERBOSE_LOG("[%i x %i]", n, p);

	// convert matrix
	Rcpp::Matrix<RTYPE> res(n,p);
	const T* o_val = x.data() + L-1;
	for(int idx = L-1; idx>=0; --idx, --o_val)
		res[idx] = *o_val;

	// if only one row: return it as a vector
	if( n == 1 ){
		VERBOSE_LOG(" -> Vector[%i]\n", p);
		return Rcpp::Vector<RTYPE>(res.row(0));
	}
	VERBOSE_LOG("\n");

	return res;

}


typedef Array<int> oct_intArray;
/**
 * Converts an Octave Array of integer into an integer R matrix or vector.
 */
inline SEXP wrap(const Array<int>& x){
	VERBOSE_LOG("(intMatrix) -> IntegerMatrix");
	return wrapArray<INTSXP>(x);
}

/**
 * Converts an Octave Array of integer into a double R matrix or vector.
 */
inline SEXP wrap(const Array<double>& x){
	VERBOSE_LOG("(doubleMatrix) -> NumericMatrix");
	return wrapArray<REALSXP>(x);
}

/**
 * Converts an Octave numeric NDArray into a double R array.
 *
 * Currently only 3D arrays are supported.
 */
inline SEXP wrap(const NDArray& x){
	VERBOSE_LOG("(NDArray) -> Array");
	if( x.ndims() > 3 ){
		std::ostringstream err;
		err << "<NDArray> - Could not convert NDArray[" << x.ndims() << "]: only up to 3 dimensions are supported";
		WRAP_ERROR(err.str().c_str());
	}

	// copy values from the outer to inner dimensions
	int n = x.dim1();
	int p = x.dim2();
	int q = x.dim3();
	Rcpp::NumericVector res( Rcpp::Dimension(n, p, q) );
	Rcpp::NumericVector::iterator z = res.begin();
	for(int k=0; k<q; k++){
		for(int j=0; j<p; j++){
			for(int i=0; i<n; i++){
				*z = x.elem(i,j,k);
				z++;
			}
		}
	}
	return res;
}

/**
 * Converts a Cell object (i.e. Array<octave_value>) into an R list
 */
SEXP wrap(const Cell& x, bool simplify = true){

	// get array length
	int n = x.length();
	VERBOSE_LOG("wrap<Cell[%i]>", n);

	// treat string Cell objects differently
	if(  x.is_cellstr() ){
		VERBOSE_LOG(" -> CharacterVector\n");
		Rcpp::CharacterVector res(n);
		for(int i=0; i<n; i++){
			res[i] = x(i).string_value();
		}
		return( res );
	}

	// variables to track for a single unit type
	const char* elem_type = n > 0 ? x(0).type_name().c_str() : NULL;
	bool single_unit_type = n > 0;

	// wrap each element into a list
	if( n > 0 ) VERBOSE_LOG("[ ");
	Rcpp::List res(n);
	for(int i=0; i<n; i++){
		const octave_value& ov = x(i);
		SEXP v = Rcpp::wrap(ov);
		// track element type
		single_unit_type = single_unit_type && Rf_length(v) == 1 && !strcmp(elem_type, ov.type_name().c_str());
		res[i] = v;
	}
	if( n > 0 ) VERBOSE_LOG(" ]");

	// if a single unit type was detected, then unlist the result
	if( single_unit_type ){
		VERBOSE_LOG(" | Unlist<%s>\n", elem_type);
		Rcpp::Environment benv = Rcpp::Environment::base_namespace();
		Rcpp::Function unlist = benv["unlist"];
		return( unlist(res) );
	}else if( res.length() == 1 && simplify ){ // simplify Cell objects of length 1
		VERBOSE_LOG(" | Simplify\n");
		return( res[0] );
	}else{
		VERBOSE_LOG("\n");
		return( res );
	}
}

/**
 * Converts an Octave object into an R object.
 */
template <> SEXP Rcpp::wrap( const octave_value& val){

	VERBOSE_LOG("wrap<%s>", val.type_name().c_str());
	if( val.is_null_value() ){

		VERBOSE_LOG("null_value");
		return R_NilValue;

	}else if (val.is_matrix_type()) {// matrix value: row vectors are converted into R vectors

		// check if multidimensional array
		if( val.ndims() > 2 ){
			VERBOSE_LOG("(NDArray) -> Array");
			return ::wrap(val.array_value());
		}else if ( val.is_string() ){

			VERBOSE_LOG("(CellStr) -> CharacterVector");
			//const string_vector s(val.cellstr_value()); // works >= 3.4.3
			const Cell& s = val.cellstr_value();
			int n = s.length();
			if( n == 0 )
				return CharacterVector(val.string_value());

			// character vector
			SEXP res = wrap(s);
			VERBOSE_LOG("[%i]\n", Rf_length(res));
			return res;

		}else if ( val.is_char_matrix() ){
			VERBOSE_LOG("(charMatrix) -> CharacterVector");
			charMatrix m = val.char_matrix_value();
			int n = m.rows();
			CharacterVector res(n);
			for(int i=0; i<n; ++i)
				res[i] = m.row_as_string(i);
			return res;
		}
		else if ( val.is_bool_type() ){

			VERBOSE_LOG("(boolMatrix) -> LogicalMatrix");
			return wrapArray<LGLSXP>(val.bool_matrix_value());

		}else if( val.is_int32_type() || val.is_int64_type() || val.is_int16_type() || val.is_integer_type() ){

			return ::wrap(static_cast<oct_intArray>(val.int32_array_value()));

		}else if( val.is_real_type() ){
			return ::wrap(val.matrix_value());
		}else{

			std::ostringstream err;
			err << " - Octave matrix type `" << val.type_name().c_str() << "` not supported.";
			WRAP_ERROR(err.str().c_str());

		}

		return R_NilValue;

	}
	else if (val.is_string()) {// single character string

		VERBOSE_LOG("(string)\n");
		const std::string s(val.string_value());
		return CharacterVector(s);

	}else if (val.is_scalar_type()) {// single scalar value

		if ( val.is_bool_scalar() ){

			VERBOSE_LOG("(bool_value)\n");
			return wrap(val.bool_value());

		}
		else if ( val.is_integer_type() ){

			VERBOSE_LOG("(int_value)\n");
			return wrap(val.int_value());

		}else if( val.is_real_type() ){

			VERBOSE_LOG("(double_value)\n");
			return wrap(val.double_value());

		}else{

			std::ostringstream err;
			err << " - Octave scalar type `" << val.type_name().c_str() << "` not supported.";
			WRAP_ERROR(err.str().c_str());

		}

		return R_NilValue;

	} else if( val.is_map() ){ // Maps are converted into lists

		VERBOSE_LOG("(map) -> ");

		OCTAVE_MAP m = val.map_value();
		const string_vector& keys = m.keys();
		int n = keys.length();
		Rcpp::List res;
		if (keys.length () == 0){
			VERBOSE_LOG("List[0] (no names)\n");
			return res;
		}else{
			VERBOSE_LOG("NamedList[%i]:\n", n);
			int nempty = 0;
			for(int i=0; i < n; ++i){
				const string& k = keys[i];
				VERBOSE_LOG("$'%s'\t: ", k.c_str());
				if( k[0] == '\0' ){
					if( ++nempty > 1 )
						WRAP_ERROR("<NamedList> - More than one empty name in Octave map");
				}
				const Cell& cell = m.contents(k);
				if( cell.length() == 0 ){
					VERBOSE_LOG("empty\n");
					res[k] = R_NilValue;
					continue;
				}
				res[k] = ::wrap(cell);
			}
			return res;
		}

	} else if( val.is_cs_list() PRE_3_4_0(|| val.is_list()) ){

		VERBOSE_LOG("(cs_list) => List\n");
		return wrap<octave_value_list>(val.list_value());

	} else if( val.is_cell() ){// Cell objects are used for character vectors

		return( ::wrap(val.cell_value()) );

	} else{

		std::ostringstream err;
		err << " - Octave type `" << val.type_name().c_str() << "` is not supported.";
		WRAP_ERROR(err.str().c_str());
	}

	return R_NilValue;
}

/**
 * Converts an Octave list of values into an R list.
 *
 * The conversion is done by wrapping each element of the list into a Rcpp::List
 * object.
 */
template <> SEXP Rcpp::wrap( const octave_value_list& val ){

	VERBOSE_LOG("wrap<list>\n");
	// create a list of the same length
	Rcpp::List res(val.length());

	// wrap each element
	for(int i = res.length()-1; i>=0; --i)
		res(i) = wrap(val(i));

	return wrap(res);
}

/**
 * Copy an Rcpp matrix into an Octave array.
 */
template <int RTYPE, typename T> void as_OctaveMatrix( Rcpp::Matrix<RTYPE> x, Array<T>& res){

	int n = x.nrow();
	int p = x.ncol();
	res.resize(dim_vector(n, p));
	for(int idx = n*p-1; idx>=0; --idx)
		res(idx) = x[idx];

}

#define AS_ERROR(err) RcppOctave_error("as", err);

/**
 * Converts an R object into an Octave array.
 */
static octave_value as_OctaveMatrix( SEXP x ){

	using namespace Rcpp;
	VERBOSE_LOG("as<octave_value>");
	if( TYPEOF(x) == REALSXP ){
		VERBOSE_LOG("(NumericMatrix)\n");
		::Matrix res;
		as_OctaveMatrix(NumericMatrix(x), res);
		return octave_value(res);
	}
	else if( TYPEOF(x) == INTSXP ){
		VERBOSE_LOG("(IntegerMatrix)\n");
		int32NDArray res;
		as_OctaveMatrix(IntegerMatrix(x), res);
		octave_value val(res);
		return octave_value(res);
	}
	else if( TYPEOF(x) == LGLSXP ){
		VERBOSE_LOG("(LogicalMatrix)\n");
		boolMatrix res;
		as_OctaveMatrix(LogicalMatrix(x), res);
		octave_value val(res);
		return octave_value(res);
	}else if( TYPEOF(x) == STRSXP ){
		AS_ERROR(" - Character matrices are not supported");
		charMatrix res;
		return octave_value(res);
	}
	else{
		std::ostringstream err;
		err << " - R matrix type `" << TYPEOF(x) << "` is not supported.";
		AS_ERROR(err.str().c_str());
	}

	return octave_value();

}

/**
 * Copy an Rcpp vector into an Octave vector.
 *
 * The output vector is in fact a NDArray of dimension 1 x n (i.e. a row matrix).
 *
 * @param x the input Rcpp vector
 * @param res the Octave outpu array. It is resized to 1 x n before copying the
 * data from \var{x}.
 *
 */
template <int RTYPE, typename T> void as_OctaveVector( const Rcpp::Vector<RTYPE>& x, Array<T>& res){

	int n = x.length();
	res.resize(dim_vector(1, n));
	for (int i=n-1; i>=0; --i)
		res(i) = x[i];

}

/**
 * Converts an R object into an Octave object.
 */
template <> octave_value Rcpp::as( SEXP x ){

	using namespace Rcpp;
	VERBOSE_LOG("as<octave_value>");
	// get the dimensions of the input object
	SEXP dim = GET_DIM(x);

	// conversion into an OctaveMatrix
	if( Rf_length(dim) == 2 )
		return as_OctaveMatrix(x);

	if( Rf_isNull(x) ){ // NULL -> empty matrix
		VERBOSE_LOG("(NULL)\n");
		return  octave_null_matrix::instance;
	}else if (TYPEOF(x) == REALSXP) {
		VERBOSE_LOG("(NumericVector[%i])\n", Rf_length(x));
		::Matrix res;
		as_OctaveVector(NumericVector(x), res);
		return res;
	} else if (TYPEOF(x) == INTSXP) {
		VERBOSE_LOG("(IntegerVector[%i])\n", Rf_length(x));
		int32NDArray res;
		as_OctaveVector(IntegerVector(x), res);
		return res;
	} else if (TYPEOF(x) == LGLSXP) {
		VERBOSE_LOG("(LogicalVector[%i])\n", Rf_length(x));
		boolMatrix res;
		as_OctaveVector(LogicalVector(x), res);
		return res;
	} else if (TYPEOF(x) == STRSXP) {
		int n = Rf_length(x);
		if (n == 1){
			VERBOSE_LOG("(String[%i])\n", as<string>(x).length());
			return octave_value(as<string>(x));
		}else {
			VERBOSE_LOG("(CharacterVector[%i])\n", Rf_length(x));
			CharacterVector vx(x);
			Cell v = Cell(dim_vector(1, vx.length()));
			for (int i=n-1; i>=0; --i) {
				v.elem(i) = charNDArray(vx[i]);
			}
			return v;
		}
	}
	else if (TYPEOF(x) == VECSXP) { // List

		// load the list's names
		SEXP rnames = GET_NAMES(x);
		int na = Rf_length(rnames);

		if ( na == 0) {

			VERBOSE_LOG("(List) -> octave_value_list\n");
			return octave_value(as<octave_value_list>(x));

		}else{ // store as an Octave map
			VERBOSE_LOG("(NamedList[%i]) -> Octave map:\n", Rf_length(x));
			CharacterVector names(rnames);
			Rcpp::List xl(x);
			int n = xl.length();
			if( n != na )
				AS_ERROR(" - Inconsistent names and list lengths.")

			OCTAVE_MAP m(dim_vector(na, 1));
			int nempty = 0;
			for (int i=0; i<n; ++i){
				const string s(names[i]);
				VERBOSE_LOG("$'%s'\t: ", s.c_str());
				if( s[0] == '\0' && ++nempty > 1 ){
					AS_ERROR(" - Only one empty name is allowed in lists.")
				}
				m.assign(s, as<octave_value>(xl[i]));
			}
			return octave_value(m);
		}
	}
	else{
		std::ostringstream err;
		err << " - R type `" << TYPEOF(x) << "` is not supported.";
		AS_ERROR(err.str().c_str());
	}

	return octave_value();
}

template <> octave_value_list Rcpp::as( SEXP x ){

	if( TYPEOF(x) != VECSXP )
		AS_ERROR("<octave_value_list> - Invalid argument: VECSXP expected");

	int n = Rf_length(x);
	octave_value_list res;
	const Rcpp::List xl(x);
	for(int i=0; i<n; ++i)
		res(i) = as<octave_value>(xl[i]);

	return res;
}

/*
// From the ROctave package
octave_value
toOctaveValue(USER_OBJECT_ robj)
{
    octave_value val;
    int i, n;
    USER_OBJECT_ dims, names;
    dims = GET_DIM(robj);
    names = GET_NAMES(robj);

     If it has a dimension
    if(GET_LENGTH(dims)) {
      int nr, nc, j, ctr = 0;
      nr = INTEGER(dims)[0];
      nc = INTEGER(dims)[1];
      Matrix m = Matrix(nr, nc);
      for(j = 0; j < nc; j++) {
        for(i = 0; i < nr; i++) {
          if(TYPEOF(robj) == REALSXP)
            m(i, j) = REAL(robj)[ctr];
          else if(TYPEOF(robj) == INTSXP)
            m(i, j) = INTEGER(robj)[ctr];
          else if(TYPEOF(robj) == LGLSXP)
            m(i, j) = LOGICAL(robj)[ctr];
          ctr++;
        }
      }
      val = m;
      return(val);
    }

         Create an Octave struct by mapping each of the named
           elements in the R object to the same name in the Octave
           structure and working recursively.
           Should also take care of attributes; perhaps a sub-list of
           elements.

    if(TYPEOF(robj) == VECSXP) {
     if(GET_LENGTH(names)) {
      n = GET_LENGTH(names);
      Octave_map m;
      for(i = 0; i < n; i++) {
        m[CHAR_DEREF(STRING_ELT(names, i))] = toOctaveValue(VECTOR_ELT(robj, i));
      }
      val = octave_value(m);
      return(val);
     } else {
#if 0
      n = GET_LENGTH(robj);
      octave_value_list v;
      for(i = 0; i < n; i++) {
  	v(i) = toOctaveValue(VECTOR_ELT(robj, i));
      }
      val = octave_value(v);
     return(val);
#endif
     }
    }

    if(TYPEOF(robj) == REALSXP) {
	   n = Rf_length(robj);
           RowVector v = RowVector(n);
	   for(i = 0; i < n; i++) {
  	       v(i) = REAL(robj)[i];
	   }
	   val = v;
    } else if(TYPEOF(robj) == INTSXP) {
	   n = Rf_length(robj);
           RowVector v = RowVector(n);
	   for(i = 0; i < n; i++) {
  	       v(i) = INTEGER(robj)[i];
	   }
	   val = v;
    } else if(TYPEOF(robj) == LGLSXP) {
	   n = Rf_length(robj);
           RowVector v = RowVector(n);
	   for(i = 0; i < n; i++) {
  	       v(i) = LOGICAL(robj)[i];
	   }
	   val = v;
    } else if(TYPEOF(robj) == STRSXP) {
	   n = Rf_length(robj);
           if(n == 1)
             val = string(CHAR_DEREF(STRING_ELT(robj, 0)));
           else {
             string_vector v(n);
             for(i = 0; i < n; i++) {
  	       v(i) = string(CHAR_DEREF(STRING_ELT(robj, i)));
             }
             val = v;
           }
    } else {
         Create a reference to the R object.
           If we are dealing with a closure, treat as an ROctaveFunctionReference.
           Otherwise treat as a generic R reference object.
         Not quite working yet.
      if(TYPEOF(robj) == CLOSXP)
         val = new ROctaveFunctionReference(robj);
      else
         val = new ROctaveObjectReference(robj);
    }

    return(val);
}
*/

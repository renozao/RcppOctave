/**
 * Version-specific/compatibility definitions
 *
 */
#ifndef _RcppOctave_OCTAVE_COMPATIBILITY_H
#define _RcppOctave_OCTAVE_COMPATIBILITY_H

// define version-specific macros
#include "swig_octave_version.h"
#ifndef OCT_POST_3_4_0
	#if !SWIG_OCTAVE_PREREQ(3,4,0)
		#define OCT_POST_3_4_0 -1
	#else
		#define OCT_POST_3_4_0 1
	#endif
#endif

#if OCT_POST_3_4_0 < 0
#define PRE_3_4_0(x) x
#define POST_3_4_0(x)
#else
#define PRE_3_4_0(x)
#define POST_3_4_0(x) x
#endif


// define which class to use for Octave maps
#if SWIG_OCTAVE_PREREQ(3,8,0)
  #define OCTAVE_MAP octave_map
#else
  #define OCTAVE_MAP Octave_map
#endif
//

// Octave 4.0.0
#if SWIG_OCTAVE_PREREQ(4,0,0)
#define usage_error print_usage
#else
#define usage_error usage
#endif

#if !SWIG_OCTAVE_PREREQ(4,2,0) // version < 4.2.0
#define NUMEL length
#else
#define NUMEL numel
#endif

#endif

/**
 * Borrowed from SWIG
 * https://github.com/swig/swig/blob/8e6a539d89bc44e47ec75f8cf5d47f824e35c869/Lib/octave/octrun.swg
 */
#ifndef _RcppOctave_OCTAVE_VERSION_H
#define _RcppOctave_OCTAVE_VERSION_H

#include <octave/version.h>

// Macro for enabling features which require Octave version >= major.minor.patch
#define SWIG_OCTAVE_PREREQ(major, minor, patch) \
  ( (OCTAVE_MAJOR_VERSION<<16) + (OCTAVE_MINOR_VERSION<<8) + OCTAVE_PATCH_VERSION >= ((major)<<16) + ((minor)<<8) + (patch) )

// Reconstruct Octave major, minor, and patch versions for releases prior to 3.8.1
#if !defined(OCTAVE_MAJOR_VERSION)

# if !defined(OCTAVE_API_VERSION_NUMBER)

// Hack to distinguish between Octave 3.8.0, which removed OCTAVE_API_VERSION_NUMBER but did not yet
// introduce OCTAVE_MAJOR_VERSION, and Octave <= 3.2, which did not define OCTAVE_API_VERSION_NUMBER
# include <octave/ov.h>
# if defined(octave_ov_h)
# define OCTAVE_MAJOR_VERSION 3
# define OCTAVE_MINOR_VERSION 8
# define OCTAVE_PATCH_VERSION 0
# else

// Hack to distinguish between Octave 3.2 and earlier versions, before OCTAVE_API_VERSION_NUMBER existed
# define ComplexLU __ignore
# include <octave/CmplxLU.h>
# undef ComplexLU
# if defined(octave_Complex_LU_h)

// We know only that this version is prior to Octave 3.2, i.e. OCTAVE_API_VERSION_NUMBER < 37
# define OCTAVE_MAJOR_VERSION 3
# define OCTAVE_MINOR_VERSION 1
# define OCTAVE_PATCH_VERSION 99

# else

// OCTAVE_API_VERSION_NUMBER == 37
# define OCTAVE_MAJOR_VERSION 3
# define OCTAVE_MINOR_VERSION 2
# define OCTAVE_PATCH_VERSION 0

# endif // defined(octave_Complex_LU_h)

# endif // defined(octave_ov_h)

// Correlation between Octave API and version numbers extracted from Octave's
// ChangeLogs; version is the *earliest* released Octave with that API number
# elif OCTAVE_API_VERSION_NUMBER >= 48
# define OCTAVE_MAJOR_VERSION 3
# define OCTAVE_MINOR_VERSION 6
# define OCTAVE_PATCH_VERSION 0

# elif OCTAVE_API_VERSION_NUMBER >= 45
# define OCTAVE_MAJOR_VERSION 3
# define OCTAVE_MINOR_VERSION 4
# define OCTAVE_PATCH_VERSION 1

# elif OCTAVE_API_VERSION_NUMBER >= 42
# define OCTAVE_MAJOR_VERSION 3
# define OCTAVE_MINOR_VERSION 3
# define OCTAVE_PATCH_VERSION 54

# elif OCTAVE_API_VERSION_NUMBER >= 41
# define OCTAVE_MAJOR_VERSION 3
# define OCTAVE_MINOR_VERSION 3
# define OCTAVE_PATCH_VERSION 53

# elif OCTAVE_API_VERSION_NUMBER >= 40
# define OCTAVE_MAJOR_VERSION 3
# define OCTAVE_MINOR_VERSION 3
# define OCTAVE_PATCH_VERSION 52

# elif OCTAVE_API_VERSION_NUMBER >= 39
# define OCTAVE_MAJOR_VERSION 3
# define OCTAVE_MINOR_VERSION 3
# define OCTAVE_PATCH_VERSION 51

# else // OCTAVE_API_VERSION_NUMBER == 38
# define OCTAVE_MAJOR_VERSION 3
# define OCTAVE_MINOR_VERSION 3
# define OCTAVE_PATCH_VERSION 50

# endif // !defined(OCTAVE_API_VERSION_NUMBER)

#endif // !defined(OCTAVE_MAJOR_VERSION)

#endif

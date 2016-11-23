// Octave modules providing Octave utility functions used by the R package
// RcppOctave ()
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

#include "../compatibility.h"
#if !SWIG_OCTAVE_PREREQ(4,2,0) // version < 4.2.0
	#include <octave/config.h>
	#include <octave/oct-obj.h>
#else
	#include <octave/ovl.h>
#endif
#include <octave/defun-dld.h>
#include <octave/ov-null-mat.h>


/**
 * Simply returns its argument unchanged
 */
DEFUN_DLD (identity, args, nargout,
"USAGE: obj = identity(obj)\n\n"
"Simply returns its argument unchanged.\n")
{
  // list of return values
  octave_value_list retval;

  // number of arguments supplied
  int nargs = args.length ();

  // if wrong arguments, show message
  if (nargs != 1) {
    usage_error("Wrong number of argument: expecting a single argument.\nTry 'help identity' for info.");
    // and return empty
    return retval;
  }

  return args(0);
}

static octave_value _RCPPOCTAVE_UUID("");
/** Get/Sets RcppOctave Call UUID
 *
 */
DEFUN_DLD (octave_uuid, args, nargout,
"USAGE: obj = octave_uuid(value)\n\n"
"Sets octave call uuid returning old value, or get current value if called with no arguments.\n")
{
  // list of return values
  octave_value_list retval;

  // number of arguments supplied
  int nargs = args.length ();

  // if wrong arguments, show message
  if ( !nargs ) {
    // and return empty
    return _RCPPOCTAVE_UUID;
  }else{
	  octave_value old(_RCPPOCTAVE_UUID);
	  _RCPPOCTAVE_UUID = args(0);
	  return old;
  }
}

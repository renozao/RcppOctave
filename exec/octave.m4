# Autoconf macro definitions for Windows configure scripts of RcppOctave
## Copyright (C) 2015 Renaud Gaujoux

# START
echo -n "Loading Windows autoconf-like macros for Octave ... "

# configuration path variables from mkoctfile 
AC_OCTAVE_MKCONFIG () {
	
	outvar="$1"
	if test -z "${outvar}"; then outvar="$2"; fi
	eval "${outvar}=\"`\"${RSCRIPT}\" -e '
			source(\"exec/m4.R\"); 
			var <- var0 <- do.call(ac_prog_varpath, c(as.list(commandArgs(TRUE)), intern=TRUE))
			homevar <- Sys.getenv(c(\"OCTCONFIG_HOME\", \"MKOCT_HOME\"))
			if( all(nzchar(homevar)) && !all(homevar == homevar[1L]) ){
				var <- gsub(homevar[2L], homevar[1L], var, fixed = TRUE)
			}
			message(var, if( var != var0 ) \" [*]\")
    		cat(var)
			' \"${OCTAVE_MKOCTFILE}\" \"$2\" \"mkoctfile $3\"`\""
	AC_SUBST ${outvar}
}

# "raw" configuration variables from mkoctfile
AC_OCTAVE_MKCONFIG_RAW () {
	AC_PROG_VAR "$1" "${OCTAVE_MKOCTFILE}" "$2" "mkoctfile $3"
}

# configuration variables from octave-config
AC_OCTAVE_CONFIG () {
	AC_PROG_VAR "$1" "${OCTAVE_CONFIG}" "$2" "Octave $3"
}
# configuration path variables from octave-config
AC_OCTAVE_CONFIG_PATH () {
	AC_PROG_VARPATH "$1" "${OCTAVE_CONFIG}" "$2" "Octave path to $3"
}


AC_CC_COMPATIBLE_OCTAVE () {

	outvar="$1"
	if test -z "${outvar}"; then outvar="$3"; fi
	eval "${outvar}=\"`\"${RSCRIPT}\" -e 'source(\"exec/m4.R\"); do.call(ac_cc_compatible_octave, as.list(commandArgs(TRUE)))' \"$2\" \"$3\" \"$4\"`\""
	AC_SUBST ${outvar}
	
}


# Initialize Octave homes: necessary for subsequent calls to AC_OCTAVE_MKCONFIG
# to return the correct results
if [ "x{$OCTAVE_CONFIG}" != "x" ]; then
	echo ""
	echo -n "  * "
	AC_OCTAVE_CONFIG_PATH OCTCONFIG_HOME OCTAVE_HOME "home directory"
fi
if [ "x{$OCTAVE_MKOCTFILE}" != "x" ]; then
	echo -n "  * "
	AC_OCTAVE_MKCONFIG MKOCT_HOME OCTAVE_HOME "home directory"
fi
# DONE
echo "OK"

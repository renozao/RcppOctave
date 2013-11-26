## Process this file with autoconf to produce a configure script.
##
## Configure.in for RcppOctave
##
## Borrowed and adapted from the file configure.in for RcppGSL
## Copyright (C) 2010 Romain Francois and Dirk Eddelbuettel
## Licensed under GNU GPL 2 or later
##
## Copyright (C) 2011 Renaud Gaujoux

AC_INIT([RcppOctave], 0.13.5)

# Checks for common programs using default macros
AC_PROG_CC
AC_PROG_CXX
AC_LANG(C++)

# You will very likely need to ensure that the same C compiler and compiler 
# flags are used in the configure tests as when compiling R or your package. 
# Under a Unix-alike, you can achieve this by including the following fragment 
# early in configure.ac
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "RcppOctave - configure: could not determine R_HOME"
  exit 1
fi

# check R is a shared library
R_LDFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config --ldflags 2>/dev/null`
R_CPPFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config --cppflags 2>/dev/null`

AC_MSG_RESULT([Original R_LDFLAGS: ${R_LDFLAGS}])
AC_MSG_RESULT([Original R_CPPFLAGS: ${R_CPPFLAGS}])

# remove -fopenmp flag that breaks mkoctfile (effectively used when R shared library)
R_LDFLAGS=`echo $R_LDFLAGS | sed 's/-fopenmp//'`

has_R_shlib=no
if test -n "$R_LDFLAGS"; then
   has_R_shlib=yes
fi

AC_MSG_CHECKING([whether R is a shared library])
if test "${has_R_shlib}" = no; then
	AC_MSG_RESULT(no)
	# add path to R headers 
	R_CPPFLAGS="-I'${R_INCLUDE_DIR}' -I'${R_INCLUDE_DIR}${R_ARCH}'"
else
   AC_MSG_RESULT(yes)
fi

# Mac OS support
# inspired/partially borrowed from package rgl
AC_MSG_CHECKING([type of Operating System])
OStype=`uname`
AC_MSG_RESULT(${OStype})

AC_MSG_CHECKING([whether OS is Mac OS (Darwin)])
if test "$OStype" == "Darwin" ; then
  darwin="yes"
  
  ## adapt R_LDFLAGS aimed at mkoctfile: pass them via flag -Wl
  ## multiple options must be separated by commas
  R_LDFLAGS=-Wl,"\"$R_LDFLAGS\""
  
  ## we want the *build* cputype and not the host one.
  cmd=`echo $CC $CFLAGS | grep -E 'x86_64|ppc64|-m64'`
  if test -n "$cmd"; then
    have_64bit="yes"
  else
    have_64bit="no"
  fi 
else
  darwin="no"
fi
AC_MSG_RESULT(${darwin})

AC_MSG_RESULT([Using mkoctfile with R_LDFLAGS: ${R_LDFLAGS}])
AC_MSG_RESULT([Using mkoctfile with R_CPPFLAGS: ${R_CPPFLAGS}])

# get compilation flags
if test -n "${R_HOME}"; then
  CC=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CC`
  CFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CFLAGS`
  CPPFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CPPFLAGS`
  CXXFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CXXFLAGS`
fi

# pass R flags down to makefile in modules/
AC_SUBST(R_CPPFLAGS)
AC_SUBST(R_LDFLAGS)

#RCPP_LIBS=`"${R_HOME}/bin${R_ARCH_BIN}/Rscript" -e "Rcpp:::LdFlags()"`
#RCPP_CXXFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/Rscript" -e "Rcpp:::CxxFlags()"`
#AC_MSG_RESULT([Using Rcpp LIBS: ${RCPP_LIBS}])
#AC_MSG_RESULT([Using Rcpp CXXFLAGS: ${R_CXXFLAGS}])
#AC_SUBST(RCPP_LIBS)
#AC_SUBST(RCPP_CXXFLAGS)


## Use octave-config to find arguments for compiler and linker flags
##
## Check for non-standard programs: 
## octave-config
AC_PATH_PROG([OCTAVE_CONFIG], [octave-config])
## mkoctfile
AC_PATH_PROG([OCTAVE_MKOCTFILE], [mkoctfile])

## If octave-config was found, let's use it
if test "${OCTAVE_MKOCTFILE}" != ""; then	

	AC_DEFUN([AC_OCTAVE_MKCONFIG],[ 
		AC_MSG_CHECKING([Octave $3])
		ac_mkoct_var_$2=`${OCTAVE_MKOCTFILE} --print $2`
		AC_SUBST([$1], [$ac_mkoct_var_$2])
		AC_MSG_RESULT([$$1])
	])
	
	AC_DEFUN([AC_OCTAVE_CONFIG],[ 
		AC_MSG_CHECKING([Octave $3])
		ac_octave_var_$2=`${OCTAVE_CONFIG} --print $2`
		AC_SUBST([$1], [$ac_octave_var_$2])
		AC_MSG_RESULT([$$1])
	])
	
	AC_OCTAVE_CONFIG([OCTAVE_BINDIR], [BINDIR], [Octave bin directory])
    
	# Use mkoctfile for compiler and linker arguments
    AC_OCTAVE_MKCONFIG([OCT_CPPFLAGS], [INCFLAGS], [include flags])
    AC_OCTAVE_MKCONFIG([OCTAVE_LFLAGS], [LFLAGS], [-L flags])
    AC_OCTAVE_MKCONFIG([OCTAVE_LIBS], [OCTAVE_LIBS], [libraries])
    # add to LDFLAGS
    OCT_LDFLAGS=" ${OCTAVE_LFLAGS} ${OCTAVE_LIBS}"
    
    # check Octave compilers
    AC_OCTAVE_MKCONFIG([OCTAVE_CC], [CC], [C compiler])
    AC_OCTAVE_MKCONFIG([OCTAVE_F77], [F77], [Fortran compiler])
    
    # Octave config info
    AC_OCTAVE_CONFIG([OCTAVE_PLATFORM], [CANONICAL_HOST_TYPE], [platform])
    AC_OCTAVE_CONFIG([OCTAVE_API_VERSION], [API_VERSION], [API version])
    
    # check Octave version to know whether one should infer output names:
    # it works for Octave >= 3.4.3
    AC_OCTAVE_CONFIG([OCTAVE_VERSION], [VERSION], [version])
    
    CMPV="3.4.3"
    AC_MSG_CHECKING([whether to infer output names for Octave function (>= ${CMPV})])
    AS_VERSION_COMPARE(${OCTAVE_VERSION}, ${CMPV}, [OCT_POST_3_4_0=-1], [OCT_POST_3_4_0=0], [OCT_POST_3_4_0=1])    
    AS_IF([test "${OCT_POST_3_4_0}" != "-1"], AC_MSG_RESULT([yes]), AC_MSG_RESULT([no]))
    # add to CPPFLAGS
    OCT_CPPFLAGS+=" -DOCT_POST_3_4_0=${OCT_POST_3_4_0}"

elif test "$darwin" == "no" ; then
    AC_MSG_ERROR([mkoctfile not found, is Octave installed?
    
    -> On Debian/Ubuntu please check that the required packages are installed:
    
      * For Octave 3.6 (works out of the box):
        sudo apt-get install octave liboctave-dev
        # install as usual in R
        install.packages('RcppOctave')
        
      * For Octave 3.2.4 (might require extra command)
        sudo apt-get install octave3.2 octave3.2-headers
        # requires to explicitly export Octave lib directory 
        export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:`octave-config -p OCTLIBDIR`
        # install as usual in R
        install.packages('RcppOctave')
    ])
else
	AC_MSG_ERROR([mkoctfile not found, is Octave installed?
    
    -> On Mac OS please check that the required packages are installed:
    
      * octave package from homebrew (works out of the box):
        $ brew install gfortran octave
        
        # install as usual in R
        > install.packages('RcppOctave')
    ])
fi

# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_MSG_RESULT([Using Octave LDFLAGS: ${OCT_LDFLAGS}])
AC_SUBST(OCT_LDFLAGS)
AC_MSG_RESULT([Using Octave CPPFLAGS: ${OCT_CPPFLAGS}])
AC_SUBST(OCT_CPPFLAGS)

# Module path
AC_MSG_CHECKING([RcppOctave Octave module directory])
OCT_MODULES_PATH=${R_PACKAGE_DIR}/modules  
AC_MSG_RESULT([${OCT_MODULES_PATH}])
AC_SUBST(OCT_MODULES_PATH)

# Configure libRrng
#AC_CONFIG_SUBDIRS([src/modules])

AC_OUTPUT(R/config-vars.R)
AC_OUTPUT(src/Makevars)
AC_OUTPUT(src/modules/Makefile)


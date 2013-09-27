/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2010  The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 *
 *
 *      Interfaces to POSIX date and time functions.
 */

/*
    These use POSIX functions that are not available on all platforms,
    and where they are they may be partially or incorrectly
    implemented.  A number of lightweight alternatives are supplied,
    but generally timezone support is only available if the OS
    supplies it.  However, as these are now also mandated by C99, they
    are almost universally available, albeit with more room for
    implementation variations.

    A particular problem is the setting of the timezone TZ on
    Unix/Linux.  POSIX appears to require it, yet older Linux systems
    do not set it and do not give the correct results/crash strftime
    if it is not set (or even if it is: see the workaround below).  We
    use unsetenv() to work around this: that is a BSD construct but
    seems to be available on the affected platforms.

    Notes on various time functions:

    The current (2008) POSIX recommendation to find the calendar time
    is to call clock_gettime(), defined in <time.h>.  This may also be
    used to find time since some unspecified starting point
    (e.g. machine reboot), but is not currently so used in R.  It
    returns in second and nanoseconds, although not necessarily to
    more than clock-tick accuracy.

    The previous POSIX recommendation was gettimeofday(), defined in
    <sys/time.h>.  This returns in seconds and microseconds (with
    unspecified granularity).

    Many systems (including AIX, FreeBSD, Linux, Solaris) have
    clock_gettime().  Mac OS X and Cygwin have gettimeofday().

    Function time() is C99 and defined in <time.h>.  C99 does not
    mandate the units, but POSIX does (as the number of seconds since
    the epoch: although not mandated, time_t seems always to be an
    integer type).

    Function clock() is C99 and defined in <time.h>.  It measures CPU
    time at CLOCKS_PER_SEC: there is a small danger of integer
    overflow.

    Function times() is POSIX and defined in <sys/times.h>.  It
    returns the elapsed time in clock ticks, plus CPU times in a
    struct tms* argument (also in clock ticks).

    More precise information on CPU times may be available from the
    POSIX function getrusage() defined in <sys/resource.h>.  This
    returns the same time structure as gettimeofday() and on some
    systems offers millisecond resolution.
 */

// libRrng headers
#include "config.h"
#include "libRrng.h"
//END_libRrng

/* needed on Windows to avoid redefinition of tzname as _tzname */
#define _NO_OLDNAMES
#include <time.h>
#undef _NO_OLDNAMES

#ifdef Win32
#define gmtime R_gmtime
#define localtime R_localtime
#define mktime R_mktime
extern struct tm*  gmtime (const time_t*);
extern struct tm*  localtime (const time_t*);
extern time_t mktime (struct tm*);
#else
#include "sys/time.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for getpid */
#endif

unsigned int TimeToSeed(void)
{
    unsigned int seed, pid = getpid();
//#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_REALTIME)
//    {
//	struct timespec tp;
//	clock_gettime(CLOCK_REALTIME, &tp);
//	seed = ((uint64_t) tp.tv_nsec << 16) ^ tp.tv_sec;
//    }
//#elif defined(HAVE_GETTIMEOFDAY)
#if defined(HAVE_GETTIMEOFDAY)
    {
	struct timeval tv;
	gettimeofday (&tv, NULL);
	seed = ((uint64_t) tv.tv_usec << 16) ^ tv.tv_sec;
    }
#else
    /* C89, so must work */
    seed = (Int32) time(NULL);
#endif
    seed ^= (pid <<16);
    return seed;
}

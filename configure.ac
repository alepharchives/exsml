AC_PREREQ(2.61)
AC_INIT([runtime/interp.c])

VERSION_MAJOR=0
VERSION_MINOR=1
VERSION_FULL="$VERSION_MAJOR.$VERSION_MINOR"
VERSION="$VERSION_FULL"

AC_SUBST([VERSION_MAJOR])
AC_SUBST([VERSION_MINOR])
AC_SUBST([VERSION_FULL])
AC_SUBST([VERSION])

AM_INIT_AUTOMAKE(mosml-llvm, [$VERSION_FULL])

AC_CONFIG_SRCDIR([runtime/interp.c])
AC_LANG([C])

## Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_RANLIB
AM_CONFIG_HEADER(config.h)

## Checks for libs
AC_CHECK_LIB(m, cos)
AC_CHECK_FUNCS([strerror])


## Check for various platform specific stuff needed in the mosml runtime
# Endianness, define WORDS_BIGENDIAN on Big endian bastards
AC_C_BIGENDIAN

# AC_C_CHAR_UNSIGNED <- forget this one and use the thing below
#include <limits.h>
#if CHAR_MIN == 0
# define CHAR_UNSIGNED 1
#endif

# Sizes of different things:
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([long *])

# AC_CHECK_ALIGNOF <- perhaps to be used



AC_OUTPUT(
          Makefile
	  runtime/Makefile
)
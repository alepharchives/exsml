AC_PREREQ(2.61)
AC_INIT([runtime/interp.c])
AC_CONFIG_AUX_DIR(config)

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
AC_CONFIG_MACRO_DIR([m4])
AC_LANG([C])

## Libtool
AC_PROG_LIBTOOL

## Checks for programs.
AC_PROG_CC
AC_PROG_AWK
AC_PROG_MKDIR_P
AC_PROG_SED
AC_PROG_INSTALL
AM_CONFIG_HEADER(config.h)

## Checks for libs
AC_CHECK_LIB(m, cos)
AC_CHECK_LIB(dl, dlopen)
AC_CHECK_HEADER(unistd.h)
AC_CHECK_HEADER(dirent.h)
AC_CHECK_HEADER(sys/param.h)
AC_CHECK_HEADERS([asm/sigcontext.h])

AC_CHECK_FUNCS([getgroups])

AC_FUNC_CHOWN
AC_FUNC_GETGROUPS

## Check for various platform specific stuff needed in the mosml runtime
# Endianness, define WORDS_BIGENDIAN on Big endian bastards
AC_C_BIGENDIAN

# Sizes of different things:
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([long *])

AC_TYPE_GETGROUPS
AC_CHECK_ALIGNOF([double])

AC_DEFUN([AC_FUNC_MEMMOVE],
[AC_CHECK_FUNCS(memmove)
AC_MSG_CHECKING(for working memmove)
AC_CACHE_VAL(ac_cv_have_working_memmove,
[AC_TRY_RUN(
[#include <stdio.h>

int main(void)
{
    char buf[10];
    strcpy (buf, "01234567");
    memmove (buf, buf + 2, 3);
    if (strcmp (buf, "23434567"))
        exit (1);
    strcpy (buf, "01234567");
    memmove (buf + 2, buf, 3);
    if (strcmp (buf, "01012567"))
        exit (1);
    exit (0);
}], ac_cv_have_working_memmove=yes, ac_cv_have_working_memmove=no, ac_cv_have_working_memmove=cross)])
AC_MSG_RESULT([$ac_cv_have_working_memmove])
if test x$ac_cv_have_working_memmove != "xyes"; then
  AC_LIBOBJ(memmove)
  AC_MSG_WARN([Replacing missing/broken memmove.])
  AC_DEFINE(PREFER_PORTABLE_MEMMOVE, 1, "enable replacement memmove if system memmove is broken or missing")
fi])



AC_DEFUN([AX_CHECK_ALIGNED_ACCESS_REQUIRED],
[AC_CACHE_CHECK([if pointers to integers require aligned access],
  [ax_cv_have_aligned_access_required],
  [AC_TRY_RUN([
#include <stdio.h>
#include <stdlib.h>

int main()
{
  char* string = malloc(40);
  int i;
  for (i=0; i < 40; i++) string[[i]] = i;
  {
     void* s = string;
     int* p = s+1;
     int* q = s+2;

     if (*p == *q) { return 1; }
  }
  return 0;
}
              ],
     [ax_cv_have_aligned_access_required=yes],
     [ax_cv_have_aligned_access_required=no],
     [ax_cv_have_aligned_access_required=no])
  ])
if test "$ax_cv_have_aligned_access_required" = yes ; then
  AC_DEFINE([HAVE_ALIGNED_ACCESS_REQUIRED], [1],
    [Define if pointers to integers require aligned access])
fi
])

AX_CHECK_ALIGNED_ACCESS_REQUIRED
AC_FUNC_MEMMOVE

## Git version
if test -d "${GIT_DIR:-${ac_top_srcdir:-./}/.git}" ; then
    GITHEAD=`git describe 2>/dev/null`

    if test -z ${GITHEAD} ; then
        GITHEAD=`git rev-parse HEAD`
    fi

    if test -n "`git diff-index -m --name-only HEAD`" ; then
        GITHEAD="${GITHEAD}-dirty"
    fi
fi


AC_MSG_CHECKING([for git version])
AC_MSG_RESULT([$GITHEAD])
AC_SUBST([GITHEAD])

GENERATED_FILE=misc/generated-file.txt
AC_SUBST_FILE(GENERATED_FILE)

if test "$ac_cv_sizeof_long_p" -eq 8 ; then
   MAX_ARRAY_LEN=18014398509481983
   INT_PRECISION=63
   INT_MIN='~4611686018427387904'
   INT_MAX=4611686018427387903
else
   MAX_ARRAY_LEN=4194303
   INT_PRECISION=31
   INT_MIN='~1073741824'
   INT_MAX=1073741823
fi
AC_SUBST(MAX_ARRAY_LEN)
AC_SUBST(INT_PRECISION)
AC_SUBST(INT_MIN)
AC_SUBST(INT_MAX)

AC_OUTPUT(
          Makefile
	  compiler/Makefile
	  mosmllib/Makefile
	  mosmllib/Array.sml
	  mosmllib/Int.sml
	  mosmllib/Weak.sml
	  runtime/Makefile
	  mosmlyac/Makefile
	  libsml_crypt/Makefile
	  libsml_unix/Makefile
	  libsml_socket/Makefile
)

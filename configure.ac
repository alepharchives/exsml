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

AM_INIT_AUTOMAKE(ex-sml, [$VERSION_FULL])

AC_CONFIG_SRCDIR([runtime/interp.c])
AC_CONFIG_MACRO_DIR([m4])
AC_LANG([C])

## Default installation areas; can be overridden.
AC_ARG_VAR([exsmllibdir],[location of the exsml standard library [$datarootdir/exsml]])
exsmllibdir=$datarootdir/exsml/exsmllib/
AC_ARG_VAR([exsmllibexecdir],[location of the exsml byte executables [$libexecdir/exsml]])
exsmllibexecdir=$libexecdir/exsml

## Libtool
AC_PROG_LIBTOOL

## Checks for programs.
AC_PROG_CC
AC_PROG_AWK
AC_PROG_MKDIR_P
AC_PROG_SED
AC_PROG_INSTALL
AM_CONFIG_HEADER(config.h)

AC_DEFUN([AC_PROG_PDFLATEX],[
AC_CHECK_PROGS(pdflatex,[pdflatex],no)
export pdflatex;
if test $pdflatex = "no" ;
then
	AC_MSG_ERROR([Unable to find a PDFLaTeX application]);
fi
AC_SUBST(pdflatex)
])

AC_PROG_PDFLATEX

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

## Check for various platform specific stuff needed in the exsml runtime
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
AC_DEFINE_UNQUOTED([GIT_HEAD], "${GITHEAD}", [Git commit version])

GENERATED_FILE=misc/generated-file.txt
AC_SUBST_FILE(GENERATED_FILE)

## WORD_MAXSIZE is the same as MAX_ARRAY_LEN, but given as an expression
if test "$ac_cv_sizeof_long_p" -eq 8 ; then
   MAX_ARRAY_LEN=18014398509481983
   MAX_STRING_LEN=144115188075855863
   INT_PRECISION=63
   INT_MIN='~4611686018427387904'
   INT_MAX=4611686018427387903
   WORD_WORDSIZE=63
   WORD_MAXPOS='0wx3FFFFFFFFFFFFFFF'
   AC_DEFINE([WORD_MAXSIZE], [((1L << 54) - 1)],
             [The maximal size of a word in this architecture]) 
else
   MAX_ARRAY_LEN=4194303
   MAX_STRING_LEN=16777211
   INT_PRECISION=31
   INT_MIN='~1073741824'
   INT_MAX=1073741823
   WORD_WORDSIZE=31
   WORD_MAXPOS='0wx3FFFFFFF'
   AC_DEFINE([WORD_MAXSIZE], [((1L << 22) - 1)],
             [The maximal size of a word in this architecture]) 
fi
AC_SUBST(MAX_ARRAY_LEN)
AC_SUBST(INT_PRECISION)
AC_SUBST(INT_MIN)
AC_SUBST(INT_MAX)
AC_SUBST(WORD_WORDSIZE)
AC_SUBST(WORD_MAXPOS)
AC_SUBST(MAX_STRING_LEN)

AC_DEFUN([CHECK_CFLAG], [
    current_cflags=$CFLAGS
    CFLAGS="$CFLAGS $1 -Werror"
    AC_COMPILE_IFELSE([
#include <stdio.h>
int main()
{
    printf("Goat");
    return 0;
}
    ],
    [cflags_success=yes],
    [cflags_success=no])

    CFLAGS="$current_cflags"

    if test "x$cflags_success" = "xyes" ; then
        CFLAGS="$CFLAGS $1"
        cflags_message="$cflags_message $1"
    fi
])
AC_MSG_CHECKING([for compiler flags])
CHECK_CFLAG([-Wall])
CHECK_CFLAG([-Wcast-align])
CHECK_CFLAG([-Wcast-qual])
CHECK_CFLAG([-Wextra])
CHECK_CFLAG([-Winline])
CHECK_CFLAG([-Wmissing-noreturn])
CHECK_CFLAG([-Wmissing-prototypes])
CHECK_CFLAG([-Wshadow])
CHECK_CFLAG([-Wstrict-aliasing])
CHECK_CFLAG([-Wstrict-overflow])
CHECK_CFLAG([-Wswitch-default])
CHECK_CFLAG([-Wswitch-enum])
CHECK_CFLAG([-Wundef])
CHECK_CFLAG([-Wunused])
CHECK_CFLAG([-fstrict-aliasing])
CHECK_CFLAG([-fstrict-overflow])
CHECK_CFLAG([-ftree-vrp])
AC_MSG_RESULT([${cflags_message}])

AC_OUTPUT(
    Makefile
    bin/Makefile
    bin/exsml
    bin/exsmlc
    compiler/Makefile
    doc/Makefile
    doc/moscow-ml-manual/Makefile 
    exsmllib/Architecture.sml
    exsmllib/Makefile
    mosmlyac/Makefile
    regression/Makefile
    runtime/Makefile
    tools/Makefile
)

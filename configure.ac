dnl vim: set sw=4 sts=4 ts=4 noet ft=config foldmethod=marker foldmarker={{{,}}} :
 
AC_PREREQ(2.61)
AC_INIT([inops/inops.cc])
 
VERSION_MAJOR=0
VERSION_MINOR=1
VERSION_FULL="$VERSION_MAJOR.$VERSION_MINOR"
VERSION="$VERSION_FULL"
 
AC_SUBST([VERSION_MAJOR])
AC_SUBST([VERSION_MINOR])
AC_SUBST([VERSION_FULL])
AC_SUBST([VERSION])
 
AM_INIT_AUTOMAKE(inops, [$VERSION_FULL])
 
AC_CONFIG_SRCDIR([inops/inops.cc])
AC_LANG([C++])
 
# Checks for programs.
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_RANLIB
 
dnl {{{ check for cxxflags
if test x = x"$LET_ME_RICE"
then
	AC_MSG_CHECKING([for go faster stripes])
	for x in "enforce-eh" "fast-math" "rtti"
	do
		if echo "${LDFLAGS} ${CFLAGS} ${CXXFLAGS}" | grep "$x" >/dev/null ; then
			AC_MSG_RESULT([$x])
			AC_MSG_ERROR([Bad ricer. No bagel.])
		fi
	done
	AC_MSG_RESULT([no])
fi
 
PALUDIS_CXXFLAGS=
AC_MSG_CHECKING([whether our compiler is iccy])
AC_COMPILE_IFELSE([
#ifndef __ICC
#error nope
#endif
],
	[cxx_compiler_icc=yes],
	[cxx_compiler_icc=no])
AC_MSG_RESULT([${cxx_compiler_icc}])
AC_DEFUN([CHECK_CXXFLAG], [
	save_CXXFLAGS=$CXXFLAGS
	CXXFLAGS="$CXXFLAGS $PALUDIS_CXXFLAGS $1 -Werror"
	AC_COMPILE_IFELSE([
#include <string>
#include <iostream>
int main(int, char **)
{
	std::string s("test");
	std::cout << s << std::endl;
}
	],
	[cxxflag_success=yes],
	[cxxflag_success=no])
	CXXFLAGS="$save_CXXFLAGS"
	if test "x$cxxflag_success" = "xyes" ; then
		PALUDIS_CXXFLAGS="$PALUDIS_CXXFLAGS $1"
		cxxflags_message="${cxxflags_message} $1"
	fi
	])
AC_MSG_CHECKING([for appropriate compiler flags])
if test "x${cxx_compiler_icc}" = "xyes" ; then
	CHECK_CXXFLAG([-Wall])
	CHECK_CXXFLAG([-wd279])
	CHECK_CXXFLAG([-wd304])
	CHECK_CXXFLAG([-wd383])
	CHECK_CXXFLAG([-wd444])
	CHECK_CXXFLAG([-wd488])
	CHECK_CXXFLAG([-wd981])
	CHECK_CXXFLAG([-wd1125])
	CHECK_CXXFLAG([-wd1418])
else
	CHECK_CXXFLAG([-Wall])
fi
AC_MSG_RESULT([${cxxflags_message}])
AC_SUBST([PALUDIS_CXXFLAGS])
dnl }}}
 
PKG_PROG_PKG_CONFIG([0.9.0])
 
dnl {{{ we need paludis
PKG_CHECK_MODULES([paludis], [paludis >= 0.25.0])
dnl }}}
 
dnl {{{ we need libpcre++
AC_CHECK_PROG(HAVE_PRCEPLUSPLUS, [pcre++-config], [yes], [no])
if test x"$HAVE_PRCEPLUSPLUS" = "xyes" ; then
	PCREPLUSPLUS_CFLAGS=`pcre++-config --cflags`
	AC_SUBST(PCREPLUSPLUS_CFLAGS)
	PCREPLUSPLUS_LIBS=`pcre++-config --libs`
	AC_SUBST(PCREPLUSPLUS_LIBS)
else
	AC_MSG_ERROR([pcre++ (http://www.daemon.de/PCRE) is required])
fi
dnl }}}
 
dnl {{{ default layout?
AC_MSG_CHECKING([for default layout type])
AC_ARG_WITH([default-layout],
			AS_HELP_STRING([--with-default-layout=layout], [Specify default layout name]),
			DEFAULT_LAYOUT=`eval echo $withval`,
			DEFAULT_LAYOUT=gentoo)
AC_MSG_RESULT([$DEFAULT_LAYOUT])
AC_SUBST([DEFAULT_LAYOUT])
AC_DEFINE_UNQUOTED([DEFAULT_LAYOUT], "$DEFAULT_LAYOUT", [Default layout name])
dnl }}}
 
dnl {{{ information to about.hh
GENERATED_FILE=misc/generated-file.txt
AC_SUBST_FILE(GENERATED_FILE)
 
BUILDUSER=`whoami`
AC_SUBST([BUILDUSER])
BUILDHOST=`hostname`
AC_SUBST([BUILDHOST])
BUILDDATE=`date +%Y-%m-%dT%H:%M:%S%z`
AC_SUBST([BUILDDATE])
dnl }}}
 
AM_CONFIG_HEADER(config.h)
AC_OUTPUT(
          Makefile
          bash-completion/Makefile
          inops/Makefile
          inops/utils/Makefile
          inops/utils/output/Makefile
          inops/utils/text/Makefile
          inops/utils/misc/Makefile
          inops/tasks/Makefile
          inops/vcs/Makefile
          inops/changelog/Makefile
)
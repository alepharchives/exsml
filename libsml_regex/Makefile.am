# Makefile for dynamic library interface to POSIX regular expressions

# The runtime system must be compiled with support for dynamic libraries.
# The regex library must be installed.

AM_CFLAGS=-fno-defer-pop -I$(top_srcdir)/runtime -I$(top_srcdir)

lib_LTLIBRARIES = libsml_regex.la
libsml_regex_la_SOURCES = libsml_regex.c


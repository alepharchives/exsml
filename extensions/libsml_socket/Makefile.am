# Makefile for dynamic library interface to sockets

# The runtime system must be compiled with support for dynamic libraries.
# The operating system must support sockets.

AM_CFLAGS = -fno-defer-pop -I$(top_srcdir)/runtime -I$(top_srcdir)
lib_LTLIBRARIES = libsml_socket.la
libsml_socket_la_SOURCES = socket.c


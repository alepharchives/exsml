ACLOCAL_AMFLAGS = -I m4
SUBDIRS = runtime mosmlyac mosmllib compiler \
	regression tools doc bin
EXTRA_DIST = autogen.sh


## Run a top-level interpreter for the compiled code
.phony: runtop
runtop: all
	$(top_builddir)/runtime/exsmlrunm \
	$(top_builddir)/compiler/mosmltop \
	-stdlib $(top_builddir)/mosmllib \
	-conservative

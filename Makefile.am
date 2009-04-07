ACLOCAL_AMFLAGS = -I m4
SUBDIRS = runtime mosmlyac mosmllib compiler libsml_crypt \
	libsml_unix libsml_socket regression tools doc
EXTRA_DIST = autogen.sh


## Run a top-level interpreter for the compiled code
.phony: runtop
runtop: all
	$(top_builddir)/runtime/camlrunm \
	$(top_builddir)/compiler/mosmltop \
	-stdlib $(top_builddir)/mosmllib \
	-conservative
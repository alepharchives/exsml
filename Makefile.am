ACLOCAL_AMFLAGS = -I m4
SUBDIRS = runtime mosmlyac exsmllib compiler \
	regression tools doc bin
EXTRA_DIST = autogen.sh


## Run a top-level interpreter for the compiled code
.phony: runtop
runtop: all
	$(top_builddir)/runtime/exsmlrunm \
	$(top_builddir)/compiler/mosmltop \
	-stdlib $(top_builddir)/exsmllib \
	-conservative

.phony: bump-compiler
bump-compiler:
	cp $(top_builddir)/compiler/mosmlcmp $(top_srcdir)/bootstrap
	cp $(top_builddir)/compiler/mosmllnk $(top_srcdir)/bootstrap
	cp $(top_builddir)/tools/mosmldep    $(top_srcdir)/bootstrap
# TODO: Lexer
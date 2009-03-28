## Makefile.am -- Process this file with automake to produce Makefile.in

EXTRA_DIST = defs $(TESTS)
MAINTAINERCLEANFILES = Makefile.in
testsubdir = testSubDir
TESTS_ENVIRONMENT = ${srcdir}/run-test-ok
TESTS = tests/hello-world.sml \
	tests/int.sml \
	tests/kittmergesort.sml \
	tests/life.sml
# tests/word.sml \   -- Currently fails on mosml

distclean-local:
	-rm -fr $(testsubdir)

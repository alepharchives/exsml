## Makefile.am -- Process this file with automake to produce Makefile.in

EXTRA_DIST = defs $(TESTS)
MAINTAINERCLEANFILES = Makefile.in
testsubdir = testSubDir
TESTS_ENVIRONMENT = ${srcdir}/run-test-ok
TESTS = tests/hello-world.sml \
	tests/int.sml \
	tests/kittmergesort.sml \
	tests/life.sml \
	tests/kitsimple.sml \
	tests/kitreynolds2.sml \
	tests/kitreynolds3.sml \
	tests/kitqsort.sml \
	tests/kitdangle3.sml \
	tests/kitfib35.sml \
	tests/kitkbjul9.sml
# tests/int-overflow.sml
# tests/word.sml \   -- Currently fails on mosml

distclean-local:
	-rm -fr $(testsubdir)

## Makefile.am -- Process this file with automake to produce Makefile.in

EXTRA_DIST = defs $(TESTS)
MAINTAINERCLEANFILES = Makefile.in
testsubdir = testSubDir
TESTS_ENVIRONMENT = top_builddir=$(top_builddir)
TESTS = run-test-1

distclean-local:
	-rm -fr $(testsubdir)

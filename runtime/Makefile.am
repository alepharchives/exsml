# Makefile for the Moscow ML 2.00 version of the Caml Light runtime system

AM_CFLAGS = -I$(top_srcdir) -Wall -Wextra -Wunused -Wstrict-aliasing

bin_PROGRAMS = camlrunm
camlrunm_SOURCES = $(C_SOURCES) $(GEN_SOURCES) $(GEN_HS)

C_SOURCES = interp.c misc.c stacks.c fix_code.c main.c \
	fail.c signals.c freelist.c major_gc.c minor_gc.c memory.c alloc.c \
	roots.c compare.c ints.c floats.c str.c io.c extern.c externcp.c \
	intern.c interncp.c hash.c sys.c meta.c parsing.c lexing.c \
	gc_ctrl.c mosml.c unix.c runtime.c md5sum.c callback.c debugger.c

# Generated sources
GEN_SOURCES = prims.c
GEN_HS = opnames.h

CLEANFILES = opnames.h prims.c

prims.c : primitives
	(echo '#include "mlvalues.h"'; \
	 echo '#include "prims.h"'; \
	 sed -e 's/.*/extern value &();/' primitives; \
	 echo 'c_primitive cprim[] = {'; \
	 sed -e 's/.*/  &,/' primitives; \
	 echo '  0 };'; \
         echo 'char * names_of_cprim[] = {'; \
	 sed -e 's/.*/  "&",/' primitives; \
	 echo '  0 };') > prims.c

etags:
	etags *.c *.h

ctags:
	ctags *.c *.h

primitives : $(C_SOURCES)
	sed -n -e '/\/\* ML \*\//s/.* \([a-zA-Z0-9_][a-zA-Z0-9_]*\) *(.*/\1/p' \
                $(C_SOURCES) | grep -v UNUSED > primitives2
	sh -c 'if cmp -s primitives primitives2; \
        then rm primitives2; \
        else mv primitives2 primitives; \
        fi'


opnames.h : instruct.h
	sed -e '/\/\*/d' \
             -e 's/enum /char * names_of_/' \
             -e 's/{$$/[] = {/' \
             -e 's/\([A-Z][A-Z_0-9]*\)/"\1"/g' instruct.h > opnames.h



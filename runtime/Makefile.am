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

debugger.c: opnames.h

prims.c : primitives.def
	${AWK} -f gen_prims_c.awk primitives.def > prims.c

etags:
	etags *.c *.h

ctags:
	ctags *.c *.h

opnames.h : instruct.h
	sed -e '/\/\*/d' \
             -e 's/enum /char * names_of_/' \
             -e 's/{$$/[] = {/' \
             -e 's/\([A-Z][A-Z_0-9]*\)/"\1"/g' instruct.h > opnames.h



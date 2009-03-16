# Makefile for the Moscow ML 2.00 version of the Caml Light runtime system

AM_CFLAGS = -I$(top_srcdir) -Wall -Wextra -Wunused -Wstrict-aliasing

bin_PROGRAMS = camlrunm
camlrunm_SOURCES = $(GEN_SOURCES) $(GEN_HS) $(C_SOURCES)

C_SOURCES = interp.c misc.c stacks.c fix_code.c main.c \
	fail.c signals.c freelist.c major_gc.c minor_gc.c memory.c alloc.c \
	roots.c compare.c ints.c floats.c str.c io.c extern.c externcp.c \
	intern.c interncp.c hash.c sys.c meta.c parsing.c lexing.c \
	gc_ctrl.c mosml.c unix.c runtime.c md5sum.c callback.c debugger.c

# Generated sources
GEN_SOURCES = prims.c instruct.c
GEN_HS = instruct.h

CLEANFILES = instruct.h instruct.c prims.c

debugger.c: instruct.h

prims.c : primitives.def
	${AWK} -f gen_prims_c.awk primitives.def > prims.c

instruct.h : opcodes.def
	${AWK} -f gen_instruct_h.awk opcodes.def > instruct.h

instruct.c : opcodes.def
	${AWK} -f gen_instruct_c.awk opcodes.def > instruct.c

etags:
	etags *.c *.h

ctags:
	ctags *.c *.h


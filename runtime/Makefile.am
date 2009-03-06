# Makefile for the Moscow ML 2.00 version of the Caml Light runtime system

AM_CFLAGS = -I$(top_srcdir) -Wall -DTHREADED

bin_PROGRAMS = camlrunm
camlrunm_SOURCES = $(C_SOURCES) $(H_SOURCES) $(GEN_SOURCES)

C_SOURCES = interp.c expand.c misc.c stacks.c fix_code.c main.c \
	fail.c signals.c freelist.c major_gc.c minor_gc.c memory.c alloc.c \
	roots.c compare.c ints.c floats.c str.c io.c extern.c externcp.c \
	intern.c interncp.c hash.c sys.c meta.c parsing.c lexing.c \
	gc_ctrl.c mosml.c unix.c runtime.c md5sum.c callback.c

H_SOURCES = jumptbl.h

# Generated sources
GEN_SOURCES = prims.c





DOBJS=$(OBJS:.o=.d.o) debugger.d.o

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

#camlrunm: $(OBJS) prims.o
#	$(LD) -o camlrunm$(EXE) prims.o $(OBJS) $(LIBS)
#	$(STRIP) camlrunm$(EXE)

#camlrunmd: $(DOBJS) prims.d.o
#	$(CC) $(CFLAGS) -o camlrunmd$(EXE) prims.d.o $(DOBJS) $(LIBS)

# clean:
# 	rm -f camlrunm$(EXE) camlrunmd$(EXE) *.o *.a
# 	rm -f primitives prims.c opnames.h jumptbl.h
# 	rm -f .debugobj/*.o

# install:
# 	${INSTALL_PROGRAM} camlrunm$(EXE) $(BINDIR)
# 	${INSTALL_PROGRAM} *.h $(INCDIR)

primitives : $(C_SOURCES)
	sed -n -e '/\/\* ML \*\//s/.* \([a-zA-Z0-9_][a-zA-Z0-9_]*\) *(.*/\1/p' \
                $(C_SOURCES) > primitives2
	sh -c 'if cmp -s primitives primitives2; \
        then rm primitives2; \
        else mv primitives2 primitives; \
        fi'


# opnames.h : instruct.h
# 	sed -e '/\/\*/d' \
#             -e 's/enum /char * names_of_/' \
#             -e 's/{$$/[] = {/' \
#             -e 's/\([A-Z][A-Z_0-9]*\)/"\1"/g' instruct.h > opnames.h

# jumptbl.h is required only if you have GCC 2.0 or later
jumptbl.h : instruct.h
	awk -f ../src/tools/make-jumptbl instruct.h > jumptbl.h

# .SUFFIXES: .d.o

# .c.d.o:
# 	cd .debugobj; $(CC) -c -g -I.. -I.. -DDEBUG $(OPTS) ../$<
# 	mv .debugobj/$*.o $*.d.o

# depend : prims.c opnames.h jumptbl.h
# 	mv Makefile Makefile.bak
# 	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak;	 \
#          gcc -MM -I.. $(OPTS) *.c; \
#          gcc -MM -I.. $(OPTS) -DDEBUG *.c | sed -e 's/\.o/.d.o/') > Makefile
# 	rm Makefile.bak
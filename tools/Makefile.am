# Unix Makefile for mosmldep

include $(top_srcdir)/Makefile.mosml
include Makefile.mosml_deps

INCLUDES=-I $(top_srcdir)/compiler
COMPFLAGS=$(INCLUDES)
LINKFLAGS=-g $(INCLUDES)

CLEANFILES = mosmldep *.ui *.uo Deppars.sig Deppars.sml

OBJS = \
	Deppars.uo Deplex.uo Mosmldep.uo

LIBOBJS = \
	Obj.uo Nonstdio.uo Lexing.uo Parsing.uo

CLIBOBJS = \
	Hasht.uo Fnlib.uo Config.uo

all: mosmldep

mosmldep: $(OBJS)
	$(MOSMLL) $(LINKFLAGS) -noheader -o mosmldep Mosmldep.uo

Deppars.sml Deppars.sig: Deppars.mlyac
	$(MOSMLYAC) Deppars.mlyac

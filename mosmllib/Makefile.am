# Unix Makefile for Moscow ML library

# Library is compiled with -imptypes, even when used with value polymorphism
COMPFLAGS=-imptypes

CLEANFILES = *.ui *.uo Mosml.sml Strbase.sml Word8Array.sml \
	Help.sml Vector.sml Word8Vector.sml

include Makefile.mosml_deps

# Make with the supplied compiler

all: Array.uo Array2.uo Arraysort.uo \
     BasicIO.uo Binaryset.uo Binarymap.uo BinIO.uo Bool.uo Byte.uo \
     Callback.uo Char.uo CharArray.uo CharVector.uo CommandLine.uo \
     Date.uo Dynarray.uo Dynlib.uo \
     FileSys.uo Gdbm.uo Gdimage.uo Help.uo \
     Int.uo Intmap.uo Intset.uo IO.uo \
     Lexing.uo List.uo ListPair.uo Listsort.uo Location.uo \
     Math.uo Misc.uo Mosml.uo Mosmlcgi.uo Mosmlcookie.uo Msp.uo Mysql.uo \
     NJ93.uo Nonstdio.uo \
     Obj.uo Option.uo OS.uo \
     Parsing.uo Path.uo Polygdbm.uo Polyhash.uo Postgres.uo PP.uo Process.uo \
     Random.uo Real.uo Regex.uo \
     Signal.uo SML90.uo Socket.uo Splaymap.uo Splayset.uo Splaytree.uo \
     Strbase.uo String.uo StringCvt.uo Substring.uo Susp.uo \
     TextIO.uo Time.uo Timer.uo \
     Unix.uo Vector.uo \
     Weak.uo Word.uo Word8.uo Word8Array.uo Word8Vector.uo

# mosmldep does not see that OS depends on FileSys, Path, Process:

OS.ui: FileSys.ui Path.ui Process.ui

## TODO: Hoist this!
CAMLRUNM=camlrunm
MOSMLC=$(CAMLRUNM) $(top_srcdir)/bootstrap/mosmlcmp \
	-stdlib $(top_srcdir)/mosmllib -P none
MOSMLLEX=mosmllex
MOSMLYAC=mosmlyac
MOSMLL=$(CAMLRUNM) mosmllnk -stdlib ../mosmllnk -P none

SUFFIXES = .sml .ui .uo .sig .mllex .mlyacc .mlp

.sig.ui:
	 $(MOSMLC) $(COMPFLAGS) $<

.mlp.sml:
	cpp -P $< > $@

.sml.uo:
	 $(MOSMLC) $(COMPFLAGS) $<

.lex.sml:
	 $(MOSMLLEX) $<

.mlyac.sml:
	 $(MOSMLYAC) $<


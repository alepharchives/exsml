# Unix Makefile for Moscow ML library

# Library is compiled with -imptypes, even when used with value polymorphism
COMPFLAGS=-imptypes

CLEANFILES = *.ui *.uo
include Makefile.mosml_deps

installexsmllibdir = $(exsmllibdir)
installexsmllib_DATA = *.ui *.uo *.sig *.sml

# Make with the supplied compiler

all: Array.uo ArraySlice.uo Array2.uo Arraysort.uo \
     BasicIO.uo Binaryset.uo Binarymap.uo BinIO.uo Bool.uo Byte.uo \
     Callback.uo Char.uo CharArray.uo CharVector.uo CharVectorSlice.uo \
     CharArraySlice.uo CommandLine.uo Date.uo Dynarray.uo Dynlib.uo \
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
     Unix.uo Vector.uo VectorSlice.uo \
     Weak.uo Word.uo Word8.uo Word8Array.uo Word8ArraySlice.uo \
     Word8Vector.uo Word8VectorSlice.uo Architecture.uo

# mosmldep does not see that OS depends on FileSys, Path, Process:

OS.ui: FileSys.ui Path.ui Process.ui

include ../Makefile.mosml

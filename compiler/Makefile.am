# Unix Makefile for Moscow ML compiler

include Makefile.mosml_deps

CLEANFILES = mosmlcmp mosmllnk mosmltop *.ui *.uo Opcodes.sml \
	Parser.sml Parser.sig Parser.output Lexer.sml Predef.sml \
	Prim_c.sml Makefile.bak

## TODO: Move this away
MOSMLC=camlrunm $(top_srcdir)/bootstrap/mosmlcmp \
	-stdlib $(top_srcdir)/mosmllib -P none
MOSMLLEX=mosmllex
MOSMLYAC=mosmlyac
MOSMLL=camlrunm $(top_srcdir)/bootstrap/mosmllnk -stdlib $(top_srcdir)/mosmllib -P none
MOSMLTOOLS= $(top_srcdir)/src/tools

SUFFIXES = .sml .ui .uo .sig .mllex .mlyacc .mlp

.sig.ui:
	 $(MOSMLC) $(COMPFLAGS) $<

.mlp.sml:
	$(CPP) $< >$@

.sml.uo:
	 $(MOSMLC) $(COMPFLAGS) $<

.lex.sml:
	 $(MOSMLLEX) $<

.mlyac.sml:
	 $(MOSMLYAC) $<

COMPFLAGS=
# Option `-g' exports the exception table for mosmltop to read:
LINKFLAGS=-g -noheader

COMP_OBJS = \
    Miscsys.uo Printexc.uo Filename.uo Fnlib.uo Sort.uo Stack.uo \
    Arg.uo Hasht.uo Memory.uo Config.uo Mixture.uo \
    Const.uo Prim.uo Instruct.uo \
    Lambda.uo Smlprim.uo Globals.uo Location.uo \
    Units.uo Types.uo Smlexc.uo Smlperv.uo Asynt.uo Asyntfn.uo \
    Parser.uo Lexer.uo Primdec.uo Infixst.uo Ovlres.uo \
    Synchk.uo Infixres.uo Elab.uo Tr_env.uo Pr_lam.uo \
    Match.uo Front.uo Back.uo Pr_zam.uo \
    Opcodes.uo Prim_opc.uo Buffcode.uo Labels.uo Code_dec.uo Reloc.uo \
    Emitcode.uo Emit_phr.uo Sigmtch.uo Compiler.uo

LLVM_OBJS = \
    LlvmOutput.ui LlvmOutput.uo LlvmSymtable.ui LlvmSymtable.uo Llvm.ui Llvm.uo

C_LIBOBJS= \
    List.uo Strbase.uo Char.uo String.uo BasicIO.uo TextIO.uo \
    Vector.uo Array.uo Misc.uo \
    StringCvt.uo Word.uo Word8.uo Word8Vector.uo CharVector.uo \
    Word8Array.uo CharArray.uo Obj.uo Nonstdio.uo \
    Lexing.uo Parsing.uo PP.uo

C_OBJS= \
    Mainc.uo

L_LIBOBJS= \
    List.uo Strbase.uo Char.uo String.uo BasicIO.uo TextIO.uo \
    Vector.uo Array.uo Misc.uo \
    StringCvt.uo Word.uo Word8.uo Word8Vector.uo CharVector.uo \
    Word8Array.uo CharArray.uo Obj.uo Nonstdio.uo PP.uo

L_OBJS= \
    Miscsys.uo Printexc.uo Filename.uo \
    Fnlib.uo Arg.uo Hasht.uo Config.uo Mixture.uo Const.uo \
    Opcodes.uo Code_dec.uo \
    Predef.uo Prim_c.uo Symtable.uo Patch.uo Tr_const.uo \
    Link.uo Readword.uo Mainl.uo

T_LIBOBJS= \
    List.uo Strbase.uo Char.uo String.uo BasicIO.uo TextIO.uo \
    Vector.uo Array.uo Misc.uo \
    StringCvt.uo Word.uo Word8.uo Word8Vector.uo CharVector.uo \
    Word8Array.uo CharArray.uo Obj.uo Nonstdio.uo \
    Substring.uo Path.uo Time.uo OS.uo FileSys.uo \
    Lexing.uo Parsing.uo PP.uo

T_OBJS= \
    Predef.uo Prim_c.uo Symtable.uo Patch.uo Tr_const.uo \
    Rtvals.uo Load_phr.uo Exec_phr.uo Smltop.uo Maint.uo

all: mosmlcmp mosmllnk mosmltop

mosmlcmp: $(COMP_OBJS) $(LLVM_OBJS) $(C_OBJS)
	$(MOSMLL) $(LINKFLAGS) -o mosmlcmp Mainc.uo

Opcodes.sml: $(top_srcdir)/runtime/opcodes.def
	${AWK} -f gen_opcodes_sml.awk $(top_srcdir)/runtime/opcodes.def > Opcodes.sml

Parser.sml Parser.sig: Parser.mlyac

mosmllnk: $(L_OBJS)
	$(MOSMLL) $(LINKFLAGS) -o mosmllnk Mainl.uo

Predef.sml : $(top_srcdir)/runtime/globals.def
	${AWK} -f gen_predef_sml.awk $(top_srcdir)/runtime/globals.def \
	  > Predef.sml

Prim_c.sml : $(top_srcdir)/runtime/primitives.def
	${AWK} -f gen_prim_c_sml.awk $(top_srcdir)/runtime/primitives.def \
	 > Prim_c.sml

mosmltop: $(COMP_OBJS) $(T_OBJS)
	$(MOSMLL) $(LINKFLAGS) -o mosmltop Maint.uo 


#depend: Filename.sml Config.sml Opcodes.sml Parser.sml Parser.sig Lexer.sml \
#    Predef.sml Prim_c.sml
#	rm -f Makefile.bak
#	mv Makefile Makefile.bak
#	$(MOSMLCUT) < Makefile.bak > Makefile
#	$(MOSMLDEP) >> Makefile

#regress:
#	echo "building current lib"
#	cd ../mosmllib; make -s current
#	echo "testing current lib"
#	cd ../mosmllib/test; make -s current  || echo "results differ"
#	echo "testing current test"
#	cd ../test; make -s current || echo "results differ"
#	echo "testing current compiler test"
#	cd test; make -s current || echo "results differ"


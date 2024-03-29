(* mosmldep -- (no arguments)
	       computes dependencies in a Moscow ML source directory,
	       only works correctly if all sources are
	       "flat" structure mode units.
   mosmldep (-structure | -toplevel | <unit>)+
            -- constructs dependencies from the ordered list of
               modes and units

   Handles strings and nested comments correctly; normalizes file names
   under DOS.

   Usage: mosmldep
          mosmldep (-structure | -toplevel | <unit>)+

*)

fun manglefilename s = s

open BasicIO List

open CommandLine (* cvr *)

(* Lexer of stream *)

fun createLexerStream (is : instream) =
  Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)
;

fun parsePhraseAndClear parsingFun lexingFun lexbuf =
  let val phr =
    parsingFun lexingFun lexbuf
    handle x => (Parsing.clearParser(); raise x)
  in
    Parsing.clearParser();
    phr
  end;

val parseFile =
  parsePhraseAndClear Deppars.MLtext Deplex.Token;

fun addExt s ext = s ^ "." ^ ext

local
    fun say s = (output(std_out, s); flush_out std_out)
    val col = ref 0
    and res = ref [];

    fun outstring s =
	if !col + size s >= 76 then
	    (print ("\\\n    " ^ s ^ " ");
	     col := 5 + size s)
	else
	    (print (s ^ " ");
	     col := !col + size s + 1);
in
    fun outname s =
	if FileSys.access (addExt s "sig", []) then
	    res := addExt s "ui" :: !res
	else if FileSys.access (addExt s "sml", []) then
	    res := addExt s "uo" :: !res
        else ();

    fun beginentry objext target =
	let val targetname = addExt target objext
	in
	    res := [targetname ^ ":"];
	    if objext = "uo" andalso FileSys.access(addExt target "sig", [])
	    then res := addExt target "ui" :: !res else ()
	end;


    fun endentry () =
	if length(!res) > 1 then
	    (col := 0;
	     app outstring (rev (!res));
	     print "\n")
	else ();

    fun begincommand objext target =
	let val targetname = addExt target objext
	in
	    res := [targetname ^ ":"];
	    if objext = "ui" andalso FileSys.access(addExt target "sig", [])
	    then res := addExt target "sig" :: !res else ();
	    if objext = "uo"
	    then res := addExt target "sml" :: !res else ();
	    if objext = "uo" andalso FileSys.access(addExt target "sig", [])
	    then res := addExt target "ui" :: !res else ()
	end;

    fun addcommand mode context unit srcext =
	let fun outcontext [] = ()
	    |   outcontext (unit::units) =
		(outcontext units;
	         outstring (addExt unit "ui"))
	in
	 (col := 0;
	  app outstring (rev (!res));
	  print "\n";
	  print "\t$(MOSMLC) ";
	  print mode;
	  print " ";
	  outcontext context;
	  outstring (addExt unit srcext);
	  print "\n")
	end
end;

fun read srcext objext filename =
    let val is       = open_in (addExt filename srcext)
	val lexbuf   = createLexerStream is
	val mentions = Hasht.new 37 : (string, unit) Hasht.t
	val names    = parseFile lexbuf
    in
	beginentry objext (manglefilename filename);
	app (fn name => Hasht.insert mentions name ()) names;
	Hasht.apply (fn name => fn _ => outname (manglefilename name))
	            mentions;
	close_in is;
	endentry ()
    end
    handle Parsing.ParseError _ => output(std_out, "Parseerror!\n");

fun processfile filename =
    let (* val _ = output(std_err, "Processing " ^ filename ^ "\n"); *)
	val {base, ext} = Path.splitBaseExt filename
    in
	case ext of
	    SOME "sig" => read "sig" "ui" base
	  | SOME "sml" => read "sml" "uo" base
	  | _          => ()
    end

val mode = ref "-structure";



val context = ref ([]:string list) (* in reverse order *)

fun outpredecessor [] = ()
  | outpredecessor (unit::_) = outname unit;

fun process_sig unit =
    if FileSys.access (addExt unit "sig", [])
    then
	(begincommand "ui" unit;
         case (!context) of
	   [] => ()
	 | prec_unit::_ => outname prec_unit;
	 addcommand (!mode) (rev(!context)) unit "sig")
    else ();

fun process_sml unit =
    if FileSys.access (addExt unit "sml", [])
    then
	(begincommand "uo" unit;
         case (!context) of
	   [] => ()
	 | prec_unit::_ => outname prec_unit;
	 addcommand (!mode) (rev(!context)) unit "sml")
    else ();

fun add_unit unit =
    if FileSys.access (addExt unit "sig", [])  orelse
       FileSys.access (addExt unit "sml", [])
    then context := unit::(!context) (* cvr: improve *)
    else output(std_err, "mosmldep warning: unit "^unit^" has no .sig or .sml source files (ignored)!\n")
;

fun processarg arg =
    case arg of
       "-structure" => mode := arg
    |  "-toplevel" => mode := arg
    |  unitname =>
	let val unit = manglefilename unitname
 	in  process_sig unit;
	    process_sml unit;
            add_unit unit
	end;

fun main () =
  (case CommandLine.arguments() of
     []   => (* revert to old behaviour, infer dependencies *)
  	     List.app processfile (Mosml.listDir ".")
   | args => (* build linear dependencies *)
  	     List.app processarg args)
  handle OS.SysErr (str, _) => output(std_err, str ^ "\n\n")
val _ = main ();

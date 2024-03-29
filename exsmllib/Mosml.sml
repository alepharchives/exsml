(* Mosml -- Moscow ML specific functions *)

local
    prim_val argv_    : string Vector.vector = 0 "command_line";
    prim_val getbyte_ : real -> int -> Word8.word          = 2 "get_nth_char";
    prim_val setbyte_ : real -> int -> Word8.word -> unit  = 3 "set_nth_char";
in

prim_val doubleVec : real -> Word8Vector.vector = 1 "doubletow8vec"
prim_val vecDouble_ : Word8Vector.vector -> real = 1 "w8vectodouble"

fun vecDouble (v : Word8Vector.vector) =
    if Word8Vector.length v = 8 then
	vecDouble_ v
    else
	raise Fail "Mosml.vecDouble: wrong argument length"

prim_val floatVec : real -> Word8Vector.vector  = 1 "floattow8vec"

prim_val vecFloat_ : Word8Vector.vector -> real  = 1 "w8vectofloat"

fun vecFloat (v : Word8Vector.vector) =
    if Word8Vector.length v = 4 then
	vecFloat_ v
    else
	raise Fail "Mosml.vecFloat: wrong argument length"

prim_val md5sum : string -> string = 1 "md5sum";

fun argv () =
    let fun h i res =
	if i >= 0 then h (i-1) (Vector.sub(argv_, i) :: res)
	else res
    in h (Vector.length argv_ - 1) [] end;

(* Requires Time and Timer to be loaded *)

fun time f arg =
    let open Timer
	val cputimer  = startCPUTimer ()
	val realtimer = startRealTimer ()
	fun report () =
	    let val {usr, sys} = checkCPUTimer cputimer;
		val gc = checkGCTime cputimer;
		val rea = checkRealTimer realtimer;
		fun format t = Time.toString t
	    in TextIO.print("User: "     ^ format usr ^
			    "  System: " ^ format sys ^
			    "  GC: "     ^ format gc  ^
			    "  Real: "   ^ format rea ^ "\n")
	    end
	fun x before y = x
    in
	(f arg before report ())
	handle e => (report (); raise e)
    end;

fun listDir path =
    let open FileSys
	val dir = openDir path
	fun read NONE     res = res
	  | read (SOME f) res = read (readDir dir) (f :: res)
	val files = read (readDir dir) []
    in closeDir dir; files end

datatype runresult =
    Success of string
  | Failure of string

fun run cmd args inp =
    let fun catenate xs =
	    String.concat (List.foldr (fn (s, res) => s :: " " :: res) [] xs)
	fun write filename s =
	    let open BinIO
		val os = openOut filename
	    in output(os, s); closeOut os end
	fun read filename =
	    let open BinIO
		val is  = openIn filename
		val res = inputAll is
	    in closeIn is; res end
	val infile  = FileSys.tmpName ()
	val _ = write infile (Byte.stringToBytes inp)
	val outfile = FileSys.tmpName ()
	val cmdline =
	    (* This should work for Bourne sh, POSIX sh, ksh, bash: *)
	    catenate (cmd :: List.@(args, ["<", infile, "1>", outfile,
					   "2>&1"]))
	    (* This works for bash, csh and tcsh: *)
	    (* catenate (cmd :: List.@(args, ["<", infile, "&>", outfile])) *)
	val status = Process.system cmdline
	val result = if Process.isSuccess status then
			 Success (Byte.bytesToString (read outfile))
		     else
			 ((Failure (Byte.bytesToString (read outfile)))
			  handle Io _ => Failure (cmd ^ ": command failed"))
    in
	(FileSys.remove infile)  handle SysErr _ => ();
	(FileSys.remove outfile) handle SysErr _ => ();
	result
    end

end (* local *)

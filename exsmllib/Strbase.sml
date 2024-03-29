(* Strbase -- internal utilities for String and Substring *)

val maxlen = Architecture.max_string_len

local
    prim_val sub_      : string -> int -> char         = 2 "get_nth_char";
    prim_val mkstring_ : int -> string                 = 1 "create_string";
    prim_val blit_     : string -> int -> string -> int -> int -> unit
                                                       = 5 "blit_string";
    prim_val set_nth_  : string -> int -> char -> unit = 3 "set_nth_char";

    fun str c =
	let val newstr = mkstring_ 1
	in set_nth_ newstr 0 c; newstr end;

    fun revconcat strs =
	let fun acc [] len       = len
	      | acc (v1::vr) len = acc vr (size v1 + len)
	    val len = acc strs 0
	    val newstr = if len > maxlen then raise Size else mkstring_ len
	    fun copyall to []       = () (* Now: to = 0. *)
	      | copyall to (v1::vr) =
		let val len1 = size v1
		    val to   = to - len1
		in blit_ v1 0 newstr to len1; copyall to vr end
	in copyall len strs; newstr end;

    fun rest (ss as (s, i, n)) =
	if n = 0 then ss else (s, i+1, n-1);

in


fun foldl f e (s,i,n) =
    let val stop = i+n
        fun h j res = if j>=stop then res
                      else h (j+1) (f (sub_ s j, res))
    in h i e end;

fun translate f (s,i,n) =
    let val stop = i+n
	fun h j res = if j>=stop then res
		      else h (j+1) (f(sub_ s j) :: res)
    in revconcat(h i []) end;

local
    fun scanl chop pred (s, i, n) =
	let
	    val stop = i+n
	    fun scan j = if j < stop andalso pred(sub_ s j) then scan (j+1)
			 else j
	in
	    chop (s, i, n, scan i - i)
	end
    fun scanr chop pred (s, i, n) =
	let
	    val stop = i-1
	    fun scan j = if j > stop andalso pred(sub_ s j) then scan(j-1)
			 else j
	in
	    chop (s, i, n, scan (i+n-1) - i + 1)
	end
in
    fun splitl p = scanl (fn (s, i, n, k) => ((s, i, k), (s, i+k, n-k))) p
    fun splitr p = scanr (fn (s, i, n, k) => ((s, i, k), (s, i+k, n-k))) p
    fun dropl  p = scanl (fn (s, i, n, k) => (s, i+k, n-k))              p
    fun dropr  p = scanr (fn (s, i, n, k) => (s, i, k))                  p
    fun takel  p = scanl (fn (s, i, n, k) => (s, i, k))                  p
    fun taker  p = scanr (fn (s, i, n, k) => (s, i+k, n-k))              p
end (* local *)

fun tokens isDelim ss =
    let fun findTok ss = dropl isDelim ss
        fun h (remains as (_, _, n)) res =
	    if n = 0 then List.rev res
	    else
		let val (token, aftertoken) =
		    splitl (fn c => not(isDelim c)) remains
		in h (findTok aftertoken) (token :: res) end
    in h (findTok ss) [] end;

fun fields isDelim ss =
    let fun h ss res =
	    let val (field, afterfield as (_, _, n)) =
		splitl (fn c => not(isDelim c)) ss
	    in
		if n = 0 then List.rev (field :: res)
		else h (rest afterfield) (field :: res)
	    end
    in h ss [] end;

local
    (* Conversion to and from ML and C character escape sequences *)

    exception BadEscape
    prim_val ord_ : char -> int = 1 "identity";
    prim_val chr_ : int -> char = 1 "identity";
    val maxOrd = 255			(* Must equal Char.maxOrd *)
    fun chr i = if i<0 orelse i>maxOrd then raise BadEscape else chr_ i;


    (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
    fun decval c = ord_ c - 48;
    fun digit n = chr_(48 + n);
    fun hexval c =
	if #"0" <= c andalso c <= #"9" then ord_ c - 48
	else (ord_ c - 55) mod 32;
    fun isOctDigit c = #"0" <= c andalso c <= #"7"
    fun isHexDigit c = #"0" <= c andalso c <= #"9"
	               orelse #"a" <= c andalso c <= #"f"
	               orelse #"A" <= c andalso c <= #"F"

in
fun fromMLescape getc source =
    let fun decimal cont src code =
	case getc src of
	    NONE          => raise BadEscape
	  | SOME(c, rest) => if #"0" <= c andalso c <= #"9"
			     then cont rest (code * 10 + ord_ c - 48)
			     else raise BadEscape
	val from3Dec =
	    decimal (decimal (decimal (fn src => fn code => (chr code, src))))
	fun skipform src =
	    case getc src of
		NONE              => NONE
	      | SOME(#"\\", src1) =>
		    (case getc src1 of
			 NONE              => NONE
		       | SOME(#"\\", src2) => fromMLescape getc src2
		       | res               => res)
	      | SOME(c, rest)     =>
		    if c = #" " orelse #"\009" <= c andalso c <= #"\013" then
			skipform rest
		    else NONE
    in
	case getc source of
	    NONE              => NONE
	  | SOME(#"a", rest)  => SOME(#"\007", rest) (* BEL *)
	  | SOME(#"b", rest)  => SOME(#"\008", rest) (* BS  *)
	  | SOME(#"t", rest)  => SOME(#"\009", rest) (* HT  *)
	  | SOME(#"n", rest)  => SOME(#"\010", rest) (* LF  *)
	  | SOME(#"r", rest)  => SOME(#"\013", rest) (* CR  *)
	  | SOME(#"v", rest)  => SOME(#"\011", rest) (* VT  *)
	  | SOME(#"f", rest)  => SOME(#"\012", rest) (* FF  *)
	  | SOME(#"\"", rest) => SOME(#"\"", rest)
	  | SOME(#"\\", rest) => SOME(#"\\", rest)
	  | SOME(#" ", rest)  => skipform rest
	  | SOME(#"\n", rest) => skipform rest
	  | SOME(#"\t", rest) => skipform rest
	  | SOME(#"^", rest)  =>
		(case getc rest of
		     NONE => NONE
		   | SOME(c, rest) =>
			 if #"@" <= c andalso c <= #"_" then
			     SOME(chr_ (ord_ c - 64), rest)
			 else
			     NONE)
	  | _     => SOME (from3Dec source 0)
		     handle BadEscape => NONE
    end

    fun toMLescape c =
	case c of
	    #"\\"   => "\\\\"
	  | #"\""   => "\\\""
	  | _       =>
	    if #"\032" <= c then
		if c <= #"\126" then str c
		else let val n = ord_ c
			 val newstr = mkstring_ 4
		     in
			 set_nth_ newstr 0 #"\\";
			 set_nth_ newstr 1 (digit(n div 100));
			 set_nth_ newstr 2 (digit(n div 10 mod 10));
			 set_nth_ newstr 3 (digit(n mod 10));
			 newstr
		     end
	    else
		(case c of
		     #"\007" => "\\a"			(* BEL,  7 *)
		   | #"\008" => "\\b"			(* BS,   8 *)
		   | #"\009" => "\\t"			(* HT,   9 *)
		   | #"\010" => "\\n"			(* LF,  10 *)
		   | #"\013" => "\\r"			(* CR,  13 *)
		   | #"\011" => "\\v"			(* VT,  11 *)
		   | #"\012" => "\\f"			(* FF,  12 *)
                   | _       => let val n = ord_ c
				    val newstr = mkstring_ 3
				in
				    set_nth_ newstr 0 #"\\";
				    set_nth_ newstr 1 #"^";
				    set_nth_ newstr 2 (chr_ (ord_ c + 64));
				    newstr
				end)

(* C character escape functions, 1995-10-30 *)
(* C character escape codes according to Kernighan & Ritchie: The C  *
 * Programming Language, second edition, page 193                    *)

    fun toCescape c =
	case c of
	    #"\\"   => "\\\\"
	  | #"?"    => "\\?"
	  | #"'"    => "\\'"
	  | #"\""   => "\\\""
	  | _       =>
	    if #"\032" <= c andalso c <= #"\126" then str c
	    else
		(case c of
		     #"\010" => "\\n"			(* LF,  10 *)
		   | #"\013" => "\\r"			(* CR,  13 *)
		   | #"\009" => "\\t"			(* HT,   9 *)
		   | #"\011" => "\\v"			(* VT,  11 *)
		   | #"\008" => "\\b"			(* BS,   8 *)
		   | #"\012" => "\\f"			(* FF,  12 *)
		   | #"\007" => "\\a"			(* BEL,  7 *)
                   | _       => let val n = ord_ c
				    val newstr = mkstring_ 4
				in
				    set_nth_ newstr 0 #"\\";
				    set_nth_ newstr 1 (digit(n div 64));
				    set_nth_ newstr 2 (digit(n div 8 mod 8));
				    set_nth_ newstr 3 (digit(n mod 8));
				    newstr
				end);

    fun fromCescape' getc src =		(* raises BadEscape *)
	let fun fromHex src code =
		case getc src of
		    NONE          => (chr code, src)
		  | SOME(c, rest) => if isHexDigit c
				     then fromHex rest (code * 16 + hexval c)
				     else (chr code, src)
	    fun octalOpt cont src code =
		case getc src of
		    NONE          => (chr code, src)
		  | SOME(c, rest) =>
			if #"0" <= c andalso c <= #"7"
			then cont rest (code * 8 + ord_ c - 48)
			else (chr code, src)
	    val fromOct =
		octalOpt (octalOpt (fn src => fn code => (chr code, src)))
	in
	    case getc src of
		 NONE              => raise BadEscape
	       | SOME(#"n",  src1) => (#"\n",   src1)
	       | SOME(#"r",  src1) => (#"\013", src1)
	       | SOME(#"t",  src1) => (#"\009", src1)
	       | SOME(#"v",  src1) => (#"\011", src1)
	       | SOME(#"b",  src1) => (#"\008", src1)
	       | SOME(#"f",  src1) => (#"\012", src1)
	       | SOME(#"a",  src1) => (#"\007", src1)
	       | SOME(#"\\", src1) => (#"\\",   src1)
	       | SOME(#"?",  src1) => (#"?",    src1)
	       | SOME(#"'",  src1) => (#"'",    src1)
	       | SOME(#"\"", src1) => (#"\"",   src1)
	       | SOME(#"x",  src1) =>
		     (case getc src1 of
			  NONE          => raise BadEscape
			| SOME(c, src2) =>
			      if isHexDigit c then fromHex src2 (hexval c)
			      else raise BadEscape)
	       | SOME(c,     src1) =>
			  if isOctDigit c then fromOct src1 (decval c)
			  else raise BadEscape
	end

    fun fromCescape getc src =		(* Returns a char option *)
	SOME (fromCescape' getc src)
	handle
	   BadEscape => NONE (* Illegal C escape sequence or character code *)
	 | Overflow  => NONE (* Character code far too large                *)

    fun fromCString s =
	let fun getc i = if i < size s then SOME (sub_ s i, i+1) else NONE
	    val max = ref 1
	    val tmp = ref (mkstring_ (!max))
	    fun realloc () =
		let val newmax = 2 * !max
		    val newtmp = mkstring_ newmax
		in
		    blit_ (!tmp) 0 newtmp 0 (!max);
		    max := newmax;
		    tmp := newtmp
		end
	    fun sub_string_ s start len =
		let val res = mkstring_ len
		in blit_ s start res 0 len; res end;
	    fun h len src =
		let fun addchar c = (if len >= !max then realloc () else ();
				     set_nth_ (!tmp) len c)
		in
		    case getc src of
			NONE              => sub_string_ (!tmp) 0 len
		      | SOME(#"\\", src1) =>
			    let val (c, src2) = fromCescape' getc src1
			    in addchar c; h (len+1) src2 end
		      | SOME(c,     src1) => (addchar c; h (len+1) src1)
		end
	in
	    SOME (h 0 0)
	 handle
	   BadEscape => NONE (* Illegal C escape sequence or character code *)
	 | Overflow  => NONE (* Character code far too large                *)
	end
end
end

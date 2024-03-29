(* Word -- SML Basis Library 1994-11-01, 1995-04-06, 1995-07-12,
   1996-04-01, 1999-08-05, 2000-10-24 *)

(* This unit relies on two's complement representation *)

type word = word;

val wordSize = Architecture.word_size
val max_pos = Architecture.word_maxpos

local
    prim_val orb_       : word -> word -> word = 2 "or";
    prim_val andb_      : word -> word -> word = 2 "and";
    prim_val xorb_      : word -> word -> word = 2 "xor";
    prim_val lshift_    : word -> word -> word = 2 "shift_left";
    prim_val rshiftsig_ : word -> word -> word = 2 "shift_right_signed";
    prim_val rshiftuns_ : word -> word -> word = 2 "shift_right_unsigned";

in
    prim_val toInt   : word -> int = 1 "identity"; (* NOT the SML BL fun *)
    prim_val toIntX  : word -> int = 1 "identity";
    prim_val fromInt : int -> word = 1 "identity";

    prim_val toLargeInt   : word -> int = 1 "identity";
    prim_val toLargeIntX  : word -> int = 1 "identity";
    prim_val fromLargeInt : int -> word = 1 "identity";

    prim_val toLargeWord   : word -> word = 1 "identity";
    prim_val toLargeWordX  : word -> word = 1 "identity";
    prim_val fromLargeWord : word -> word = 1 "identity";

    prim_val toLarge   : word -> word = 1 "identity";
    prim_val toLargeX  : word -> word = 1 "identity";
    prim_val fromLarge : word -> word = 1 "identity";


    fun orb (x, y)  = orb_ x y;
    fun andb (x, y) = andb_ x y;
    fun xorb (x, y) = xorb_ x y;
    fun notb x      = xorb_ x (fromInt ~1);

    val ~ = fn w => fromInt(~(toInt w))

    fun << (w, k) =
	if toInt k >= wordSize orelse toInt k < 0 then fromInt 0
	else lshift_ w k;

    fun >> (w, k) =
	if toInt k >= wordSize orelse toInt k < 0 then fromInt 0
	else rshiftuns_ w k;

    fun ~>> (w, k) =
	if toInt k >= wordSize orelse toInt k < 0 then
	    if toInt w >= 0 then	(* msbit = 0 *)
		fromInt 0
	    else			(* msbit = 1 *)
		fromInt ~1
	else
	    rshiftsig_ w k;

    val op *    : word * word -> word = op *;
    val op +    : word * word -> word = op +;
    val op -    : word * word -> word = op -;
    val op div  : word * word -> word = op div;
    val op mod  : word * word -> word = op mod;

    local
      open StringCvt
      fun skipWSget getc source = getc (dropl Char.isSpace getc source)

      (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
      fun decval c = fromInt (Char.ord c) - fromInt 48;
      fun hexval c =
	  if #"0" <= c andalso c <= #"9" then
	      fromInt (Char.ord c) - fromInt 48
	  else
	      (fromInt (Char.ord c) - fromInt 55) mod (fromInt 32);

      fun prhex i =
	  if toInt i < 10 then Char.chr(toInt (i + fromInt 48))
	  else Char.chr(toInt (i + fromInt 55));

      fun conv radix i =
	  let fun h n res =
		  if n = fromInt 0 then res
		  else h (n div radix) (prhex (n mod radix) :: res)
	      fun tostr n = h (n div radix) [prhex (n mod radix)]
	  in String.implode (tostr i) end

    in
      fun scan radix getc source =
	  let open StringCvt
	      val source = skipWS getc source
	      val (isDigit, factor) =
		  case radix of
		      BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
		    | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
		    | DEC => (Char.isDigit,                          10)
		    | HEX => (Char.isHexDigit,                       16)
	      fun dig1 NONE              = NONE
		| dig1 (SOME (c1, src1)) =
		  let fun digr res src =
		          case getc src of
			      NONE           => SOME (res, src)
			    | SOME (c, rest) =>
				  if isDigit c then
				      digr (fromInt factor * res + hexval c)
				      rest
				  else SOME (res, src)
		  in
		      if isDigit c1 then digr (hexval c1) src1
		      else NONE
		  end
	      fun getdigs after0 src =
		  case dig1 (getc src) of
		      NONE => SOME(fromInt 0, after0)
		    | res  => res
	      fun hexprefix after0 src =
		  if radix <> HEX then getdigs after0 src
		  else
		      case getc src of
			  SOME(#"x", rest) => getdigs after0 rest
			| SOME(#"X", rest) => getdigs after0 rest
			| SOME _           => SOME(fromInt 0, after0)
			| NONE             => SOME(fromInt 0, after0)
	  in
	      case getc source of
		  SOME(#"0", after0) =>
		      (case getc after0 of
			   SOME(#"w", src2) => hexprefix after0 src2
			 | SOME _           => hexprefix after0 after0
			 | NONE             => SOME(fromInt 0, after0))
		| SOME _ => dig1 (getc source)
		| NONE   => NONE
	  end;

      fun fmt BIN = conv (fromInt  2)
	| fmt OCT = conv (fromInt  8)
	| fmt DEC = conv (fromInt 10)
	| fmt HEX = conv (fromInt 16)

      fun toString w   = conv (fromInt 16) w
      fun fromString s = scanString (scan HEX) s
    end (* local for string functions *)

    fun min(w1 : word, w2) = if w1 > w2 then w2 else w1;
    fun max(w1 : word, w2) = if w1 > w2 then w1 else w2;
    fun compare (x, y: word) =
	if x<y then LESS else if x>y then GREATER else EQUAL;
    val op >    : word * word -> bool = op >;
    val op >=   : word * word -> bool = op >=;
    val op <    : word * word -> bool = op <;
    val op <=   : word * word -> bool = op <=;

    fun toInt w =
	if w > max_pos then raise Overflow
	else toIntX w

end

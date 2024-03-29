(* Int -- new basis 1995-03-19, 1996-04-01, 2004-05-02 *)

type int = int

val precision = SOME Architecture.integer_precision;
val minInt    = SOME Architecture.int_min;
val maxInt    = SOME Architecture.int_max;

local
    open StringCvt
    (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
    fun decval c = Char.ord c - 48;
    fun hexval c =
	if #"0" <= c andalso c <= #"9" then Char.ord c - 48
	else (Char.ord c - 55) mod 32;
    fun prhex i = if i < 10 then Char.chr(i + 48) else Char.chr(i + 55)
    fun skipWSget getc source = getc (dropl Char.isSpace getc source)

    fun conv radix i =
	let fun h 0 res = res
	      | h n res = h (n div radix) (prhex (n mod radix) :: res)
	    fun tostr n = h (n div radix) [prhex (n mod radix)]
	in
	    if i < 0 then
		let val last  = ~(i mod (~radix))
		    val first = i div (~radix)
		in
		    String.implode(#"~" :: h first [prhex last])
		end
	    else
		String.implode (tostr i)
	end
in
    fun scan radix getc source =
	let open StringCvt
	    val (isDigit, factor) =
		case radix of
		    BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
		  | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
		  | DEC => (Char.isDigit,                          10)
		  | HEX => (Char.isHexDigit,                       16)
	    fun dig1 sgn NONE             = NONE
	      | dig1 sgn (SOME (c, rest)) =
		let val next_val =
		        if sgn = 1 then fn (res, hv) => factor * res + hv
		        else            fn (res, hv) => factor * res - hv
		    fun digr res src =
		        case getc src of
			    NONE           => SOME (res, src)
			  | SOME (c, rest) =>
				if isDigit c then
				    digr (next_val(res, hexval c)) rest
				else
				    SOME (res, src)
		in if isDigit c then digr (sgn * hexval c) rest else NONE end
	    fun getdigs sgn after0 inp =
		case dig1 sgn inp of
		    NONE => SOME(0, after0)
		  | res  => res
	    fun hexopt sgn NONE                 = NONE
	      | hexopt sgn (SOME(#"0", after0)) =
		if radix <> HEX then getdigs sgn after0 (getc after0)
		else
		    (case getc after0 of
			 NONE             => SOME(0, after0)
		       | SOME(#"x", rest) => getdigs sgn after0 (getc rest)
		       | SOME(#"X", rest) => getdigs sgn after0 (getc rest)
		       | inp              => getdigs sgn after0 inp)
	      | hexopt sgn inp = dig1 sgn inp
	    fun sign NONE                = NONE
	      | sign (SOME (#"~", rest)) = hexopt ~1 (getc rest)
	      | sign (SOME (#"-", rest)) = hexopt ~1 (getc rest)
	      | sign (SOME (#"+", rest)) = hexopt  1 (getc rest)
	      | sign inp                 = hexopt  1 inp
	in sign (skipWSget getc source) end;

    fun fmt BIN = conv 2
      | fmt OCT = conv 8
      | fmt DEC = conv 10
      | fmt HEX = conv 16

    (* It should hold that: toString = fmt DEC = conv 10 *)
    prim_val toString : int -> string = 1 "sml_string_of_int";

    val fromString = scanString (scan DEC)
end

fun pow (x, n) =
    let fun h 0 res = res
	  | h i res = if i mod 2 = 0 then h (i div 2) (res * res)
		      else h (i-1) (x * res)
    in
	if n < 0 then
	    if x = 0 then raise Domain else 1 div (h (~n) 1)
	else h n 1
    end

val ~       : int -> int        = ~;
val op *    : int * int -> int  = op *;
val op div  : int * int -> int  = op div;
val op mod  : int * int -> int  = op mod;

local
    prim_val quot_ : int -> int -> int = 2 "quot";
    prim_val rem_  : int -> int -> int = 2 "rem"
in
    fun quot(x, y) = quot_ x y
    fun rem(x, y)  = rem_ x y
end

val op +    : int * int -> int  = op +;
val op -    : int * int -> int  = op -;
val op >    : int * int -> bool = op >;
val op >=   : int * int -> bool = op >=;
val op <    : int * int -> bool = op <;
val op <=   : int * int -> bool = op <=;
val abs     : int -> int = abs;
fun min (x, y) = if x < y then x else y : int;
fun max (x, y) = if x < y then y else x : int;
fun sign i = if i > 0 then 1 else if i < 0 then ~1 else 0;
fun compare (x, y: int) = if x<y then LESS else if x>y then GREATER else EQUAL;

fun sameSign (i, j) = sign i = sign j;

fun toInt   i   = i;
fun fromInt i   = i;
fun toLarge   i = i;
fun fromLarge i = i;

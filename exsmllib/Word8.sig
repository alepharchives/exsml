(* Word8 -- SML Basis Library *)

type word = word8

val wordSize   : int

val orb        : word * word -> word
val andb       : word * word -> word
val xorb       : word * word -> word
val notb       : word -> word
val ~          : word -> word

val <<         : word * Word.word -> word
val >>         : word * Word.word -> word
val ~>>        : word * Word.word -> word

val +          : word * word -> word
val -          : word * word -> word
val *          : word * word -> word
val div        : word * word -> word
val mod        : word * word -> word

val >          : word * word -> bool
val <          : word * word -> bool
val >=         : word * word -> bool
val <=         : word * word -> bool
val compare    : word * word -> order

val min        : word * word -> word
val max        : word * word -> word

val toString   : word -> string
val fromString : string -> word option
val scan       : StringCvt.radix
               -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
val fmt        : StringCvt.radix -> word -> string

val toInt      : word -> int
val toIntX     : word -> int            (* with sign extension *)
val fromInt    : int -> word

val toLargeInt    : word -> int
val toLargeIntX   : word -> int         (* with sign extension *)
val fromLargeInt  : int -> word

val toLargeWord   : word -> Word.word
val toLargeWordX  : word -> Word.word   (* with sign extension *)
val fromLargeWord : Word.word -> word

(*
   [word] is the type of 8-bit words, or 8-bit unsigned integers in
   the range 0..255.

   [wordSize] equals 8.

   [orb(w1, w2)] returns the bitwise `or' of w1 and w2.

   [andb(w1, w2)] returns the bitwise `and' of w1 and w2.

   [xorb(w1, w2)] returns the bitwise `exclusive or' or w1 and w2.

   [notb w] returns the bitwise negation (one's complement) of w.

   [~ w] returns the arithmetic negation (two's complement) of w.

   [<<(w, k)] returns the word resulting from shifting w left by k
   bits.  The bits shifted in are zero, so this is a logical shift.
   Consequently, the result is 0-bits when k >= wordSize.

   [>>(w, k)] returns the word resulting from shifting w right by k
   bits.  The bits shifted in are zero, so this is a logical shift.
   Consequently, the result is 0-bits when k >= wordSize.

   [~>>(w, k)] returns the word resulting from shifting w right by k
   bits.  The bits shifted in are replications of the left-most bit:
   the `sign bit', so this is an arithmetical shift.  Consequently,
   for k >= wordSize and wordToInt w >= 0 the result is all 0-bits, and
   for k >= wordSize and wordToInt w <  0 the result is all 1-bits.

   To make <<, >>, and ~>> infix, use the declaration:
                          infix 5 << >> ~>>

   [+]
   [-]
   [*]
   [div]
   [mod] represent unsigned integer addition, subtraction,
   multiplication, division, and remainder, modulus 256.  The
   operations (i div j) and (i mod j) raise Div when j = 0.  Otherwise
   no exceptions are raised.

   [<]
   [<=]
   [>]
   [>=] compare words as unsigned integers.

   [compare(w1, w2)] returns LESS, EQUAL, or GREATER, according
   as w1 is less than, equal to, or greater than w2 (as unsigned integers).

   [min(w1, w2)] returns the smaller of w1 and w2 (as unsigned integers).

   [max(w1, w2)] returns the larger of w1 and w2 (as unsigned integers).

   [fmt radix w] returns a string representing w, in the radix (base)
   specified by radix.

     radix    description                     output format
     ------------------------------------------------------
      BIN     unsigned binary      (base  2)  [01]+
      OCT     unsigned octal       (base  8)  [0-7]+
      DEC     unsigned decimal     (base 10)  [0-9]+
      HEX     unsigned hexadecimal (base 16)  [0-9A-F]+

   [toString w] returns a string representing w in unsigned
   hexadecimal format.  Equivalent to (fmt HEX w).

   [fromString s] returns SOME(w) if a hexadecimal unsigned numeral
   can be scanned from a prefix of string s, ignoring any initial
   whitespace; returns NONE otherwise.  Raises Overflow if the scanned
   number cannot be represented as a word.  An unsigned hexadecimal
   numeral must have form, after possible initial whitespace:
       [0-9a-fA-F]+

   [scan radix {getc} charsrc] attempts to scan an unsigned numeral
   from the character source charsrc, using the accessor getc, and
   ignoring any initial whitespace.  The radix argument specifies the
   base of the numeral (BIN, OCT, DEC, HEX).  If successful, it
   returns SOME(w, rest) where w is the value of the numeral scanned,
   and rest is the unused part of the character source.  Raises
   Overflow if the scanned number cannot be represented as a word.  A
   numeral must have form, after possible initial whitespace:

     radix    input format
     -------------------------------------
      BIN     (0w)?[0-1]+
      OCT     (0w)?[0-7]+
      DEC     (0w)?[0-9]+
      HEX     (0wx|0wX|0x|0X)?[0-9a-fA-F]+

   [toInt w] returns the integer in the range 0..255 represented by w.

   [toIntX w] returns the signed integer (in the range ~128..127)
   represented by bit-pattern w.

   [fromInt i] returns the word holding the 8 least significant bits of i.

   [toLargeInt w] returns the integer in the range 0..255 represented by w.

   [toLargeIntX w] returns the signed integer (in the range ~128..127)
   represented by bit-pattern w.

   [fromLargeInt i] returns the word holding the 8 least significant bits of i.

   [toLargeWord w] returns the Word.word value corresponding to w.

   [toLargeWordX w] returns the Word.word value corresponding to w,
   with sign extension.  That is, the 8 least significant bits of the
   result are those of w, and the remaining bits are all equal to the
   most significant bit of w: its `sign bit'.

   [fromLargeWord w] returns w modulo 256.
*)

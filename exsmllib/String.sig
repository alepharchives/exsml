(* String -- SML Basis Library *)

    type char = Char.char
    type string = string
    val maxSize   : int
    val size      : string -> int
    val sub       : string * int -> char
    val extract   : string * int * int option -> string
    val substring : string * int * int -> string
    val ^         : string * string -> string
    val concat    : string list -> string
    val concatWith : string -> string list -> string
    val str       : char -> string
    val implode   : char list -> string
    val explode   : string -> char list
    val map       : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens    : (char -> bool) -> string -> string list
    val fields    : (char -> bool) -> string -> string list
    val isPrefix  : string -> string -> bool
    val isSubstring : string -> string -> bool
    val isSuffix    : string -> string -> bool
    val compare   : string * string -> order
    val collate   : (char * char -> order) -> string * string -> order

    val <  : string * string -> bool
    val <= : string * string -> bool
    val >  : string * string -> bool
    val >= : string * string -> bool

    val toString    : string -> string            (* ML escape sequences *)
(*    val scan : (char, 'a) StringCvt.reader
	         -> (string, 'a) StringCvt.reader *)
    val fromString  : string -> string option     (* ML escape sequences *)
    val toCString   : string -> string            (* C escape sequences *)
    val fromCString : string -> string option     (* C escape sequences *)

(*
   [string] is the type of immutable strings of characters, with
   constant-time indexing.

   [maxSize] returns |s|, the number of characters in string s.

   [size s] is the number of characters in string s.

   [sub(s, i)] is the i'th character of s, counting from zero.
   Raises Subscript if i<0 or i>=size s.


   [extract (s, i, NONE)] is the string s[i..size s-1].
   Raises Subscript if i<0 or i>size s.

   [substring(s, i, n)] is the string s[i..i+n-1].  Raises Subscript
   if i<0 or n<0 or i+n>size s.  Equivalent to extract(s, i, SOME n).

   [extract (s, i, SOME n)] is the string s[i..i+n-1].
   Raises Subscript if i<0 or n<0 or i+n>size s.

   [s1 ^ s2] is the concatenation of strings s1 and s2. This raises
   Size if |s| + |t| > maxSize.

   [concat ss] is the concatenation of all the strings in ss.
   Raises Size if the sum of their sizes is greater than maxSize.

   [concatWith s l] returns the concatenation of the strings in the
   list l using the string s as a separator. This raises Size if the
   size of the resulting string would be greater than maxSize.

   [str c] is the string of size one which contains the character c.

   [implode cs] is the string containing the characters in the list cs.
   Equivalent to  concat (List.map str cs).

   [explode s] is the list of characters in the string s.

   [map f s] applies f to every character of s, from left to right,
   and returns the string consisting of the resulting characters.
   Equivalent to  CharVector.map f s
          and to  implode (List.map f (explode s)).

   [translate f s] applies f to every character of s, from left to
   right, and returns the concatenation of the resulting strings.
   Raises Size if the sum of their sizes is greater than maxSize.
   Equivalent to concat (List.map f (explode s)).

   [tokens p s] returns the list of tokens in s, from left to right,
   where a token is a non-empty maximal substring of s not containing
   any delimiter, and a delimiter is a character satisfying p.

   [fields p s] returns the list of fields in s, from left to right,
   where a field is a (possibly empty) maximal substring of s not
   containing any delimiter, and a delimiter is a character satisfying p.

   Two tokens may be separated by more than one delimiter, whereas two
   fields are separated by exactly one delimiter.  If the only delimiter
   is the character #"|", then
        "abc||def" contains two tokens:   "abc" and "def"
        "abc||def" contains three fields: "abc" and "" and "def"

   [isPrefix s1 s2]
   [isSubstring s1 s2]
   [isSuffix s1 s2] These functions return true if the string s1 is a
   prefix, substring, or suffix (respectively) of the string s2. Note
   that the empty string is a prefix, substring, and suffix of any
   string, and that a string is a prefix, substring, and suffix of
   itself.

   [compare (s1, s2)] does lexicographic comparison, using the
   standard ordering Char.compare on the characters.  Returns LESS,
   EQUAL, or GREATER, according as s1 is less than, equal to, or
   greater than s2.

   [collate cmp (s1, s2)] performs lexicographic comparison, using the
   given ordering cmp on characters.

   [<]
   [<=]
   [>]
   [>=] compare strings lexicographically, using the representation
   ordering on characters.

   [toString s] returns a string corresponding to s, with
   non-printable characters replaced by ML escape sequences.
   Equivalent to String.translate Char.toString.

   [scan getc strm] These functions scan their character source as a
   sequence of printable characters, converting SML escape sequences
   into the appropriate characters. They do not skip leading
   whitespace. They return as many characters as can successfully be
   scanned, stopping when they reach the end of the source or a
   non-printing character (i.e., one not satisfying isPrint), or if
   they encounter an improper escape sequence. fromString ignores the
   remaining characters, while scan returns the remaining characters
   as the rest of the stream. See the ML Basis library documentation
   for further details.

   [fromString] equivalent to 'StringCvt.scanString scan'

   [fromCString s] scans the string s as a C source program string,
   converting escape sequences into the appropriate characters.  Does
   not skip leading whitespace.

   [toCString s] returns a string corresponding to s, with
   non-printable characters replaced by C escape sequences.
   Equivalent to String.translate Char.toCString.

*)

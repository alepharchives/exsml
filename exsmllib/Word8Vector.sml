(* Word8Vector.mlp *)

prim_eqtype vector;
type elem = Word8.word;

local
    prim_val vector_ : int -> vector                 = 1 "create_string";
    prim_val sub_    : vector -> int -> elem         = 2 "get_nth_char";
    prim_val update_ : vector -> int -> elem -> unit = 3 "set_nth_char";
    prim_val blit_   : vector -> int -> vector -> int -> int -> unit
                                                     = 5 "blit_string";
    prim_val magic : 'a -> 'b = 1 "identity";
in

prim_val length : vector -> int = 1 "string_length";
val maxLen = Architecture.max_string_len;

fun fromList (vs : elem list) =
  let val n = List.length vs
      val a = if n > maxLen then raise Size else vector_ n
      fun init [] i = ()
        | init (v::vs) i = (update_ a i v; init vs (i+1))
  in (init vs 0; a) end;

fun tabulate(n, f : int -> elem) =
  if n < 0 orelse n > maxLen then raise Size else
  let val a = vector_ n
      fun init i = if i >= n then () else (update_ a i (f i); init (i+1))
  in (init 0; a) end;

fun sub(v, i) =
  if i < 0 orelse i >= length v then raise Subscript
  else sub_ v i;

fun update (v : vector, i : int, x : elem) : vector =
    let val _ = if i < 0 orelse i >= length v then raise Subscript else ()
	val stop = length v
	val newvec = vector_ stop
	fun lr j = if j < stop then (update_ newvec j (sub_ v j); lr (j+1))
		   else ()
    in lr 0; update_ newvec i x; newvec end

fun concat vecs =
    let fun acc [] len       = len
	  | acc (v1::vr) len = acc vr (length v1 + len)
	val len = acc vecs 0
	val newvec = if len > maxLen then raise Size else vector_ len
	fun copyall to []       = ()
	  | copyall to (v1::vr) =
	    let val len1 = length v1
	    in blit_ v1 0 newvec to len1; copyall (to+len1) vr end
    in copyall 0 vecs; newvec end;

fun find (p : elem -> bool) (a : vector) : elem option =
    let val stop = length a
	fun lr j =
	    if j < stop then
		if p (sub_ a j) then SOME (sub_ a j) else lr (j+1)
	    else NONE
    in lr 0 end

fun exists (p : elem -> bool) (a : vector) : bool =
    let val stop = length a
	fun lr j = j < stop andalso (p (sub_ a j) orelse lr (j+1))
    in lr 0 end

fun all (p : elem -> bool) (a : vector) : bool =
    let val stop = length a
	fun lr j = j >= stop orelse (p (sub_ a j) andalso lr (j+1))
    in lr 0 end

fun foldl f e a =
    let	val stop = length a
	fun lr j res = if j < stop then lr (j+1) (f(sub_ a j, res))
		       else res
    in lr 0 e end

fun foldr f e a =
    let	fun rl j res = if j >= 0 then rl (j-1) (f(sub_ a j, res))
		       else res
    in rl (length a - 1) e end

fun app f a =
    let val stop = length a
	fun lr j = if j < stop then (f(sub_ a j); lr (j+1))
		   else ()
    in lr 0 end

fun map (f : elem -> elem) (a : vector) : vector =
    let val stop = length a
	val newvec = vector_ stop
	fun lr j = if j < stop then (update_ newvec j (f(sub_ a j));
				     lr (j+1))
		   else ()
    in lr 0; newvec end

fun findi (p : int * elem -> bool) (a : vector) : (int * elem) option =
    let val stop = length a
	fun lr j =
	    if j < stop then
		if p (j, sub_ a j) then SOME (j, sub_ a j) else lr (j+1)
	    else NONE
    in lr 0 end

fun sliceend (a, i, NONE) =
        if i<0 orelse i>length a then raise Subscript
	else length a
  | sliceend (a, i, SOME n) =
	if i<0 orelse n<0 orelse i+n>length a then raise Subscript
	else i+n;

fun foldli f e a =
    let val stop = length a
	fun lr j res =
	    if j < stop then lr (j+1) (f(j, sub_ a j, res))
	    else res
    in lr 0 e end;

fun foldri f e a =
    let fun rl j res =
	if j >= 0 then rl (j-1) (f(j, sub_ a j, res))
	else res
    in rl (length a - 1) e end;

fun appi f a =
    let val stop = length a
	fun lr j =
	    if j < stop then (f(j, sub_ a j); lr (j+1))
	    else ()
    in lr 0 end;

fun mapi (f : int * elem -> elem) a : vector =
    let val stop = length a
	val newvec = vector_ stop
	fun lr j =
	    if j < stop then
		(update_ newvec j (f(j, sub_ a j));
		 lr (j+1))
	    else ()
    in lr 0; newvec end;

val collate  : (elem * elem -> order) -> vector * vector -> order
                                                = magic String.collate
end


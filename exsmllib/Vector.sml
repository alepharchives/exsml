(* Vector.sml -- new basis *)

type 'a vector = 'a vector;

val maxLen = Architecture.max_array_len; (* = 2^54-1, for 64-bit architectures *)

local
    prim_val vector_ : int -> 'x -> 'a vector         = 2 "make_vect";
    prim_val sub_    : 'a vector -> int -> 'a         = 2 "get_vect_item";
    prim_val update_ : 'a vector -> int -> 'a -> unit = 3 "set_vect_item";
in

prim_val length : 'a vector -> int                   = 1 "vect_length";

fun fromList (vs : 'a list) =
  let val n = List.length vs
      val a = if n > maxLen then raise Size else vector_ n () : 'a vector
      fun init [] i = ()
        | init (v::vs) i = (update_ a i v; init vs (i+1))
  in (init vs 0; a) end;

fun tabulate(n, f : int -> 'a) =
  if n < 0 orelse n > maxLen then raise Size else
  let val a = vector_ n () : 'a vector
      fun init i = if i >= n then () else (update_ a i (f i); init (i+1))

  in (init 0; a) end;

fun sub(v, i) =
    if i < 0 orelse i >= length v then raise Subscript
    else sub_ v i;

fun update (v : 'a vector, i : int, x : 'a) : 'a vector =
    let val _ = if i < 0 orelse i >= length v then raise Subscript else ()
	val stop = length v
	val newvec = vector_ stop ()
	fun lr j = if j < stop then (update_ newvec j (sub_ v j); lr (j+1))
		   else ()
    in lr 0; update_ newvec i x; newvec end

fun concat vecs =
    let fun acc [] len       = len
	  | acc (v1::vr) len = acc vr (length v1 + len)
	val len = acc vecs 0
	val newvec = if len > maxLen then raise Size else vector_ len ()
	fun copyall to []       = ()
	  | copyall to (v1::vr) =
	    let val len1 = length v1
		fun copy j =
		    if j<len1 then
			(update_ newvec (to+j) (sub_ v1 j); copy (j+1))
		    else
			()
	    in copy 0; copyall (to+len1) vr end
    in copyall 0 vecs; newvec end;

fun find (p : 'a -> bool) (a : 'a vector) : 'a option =
    let val stop = length a
	fun lr j =
	    if j < stop then
		if p (sub_ a j) then SOME (sub_ a j) else lr (j+1)
	    else NONE
    in lr 0 end

fun exists (p : 'a -> bool) (a : 'a vector) : bool =
    let val stop = length a
	fun lr j = j < stop andalso (p (sub_ a j) orelse lr (j+1))
    in lr 0 end

fun all (p : 'a -> bool) (a : 'a vector) : bool =
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

fun map (f : 'a -> 'b) (a : 'a vector) : 'b vector =
    let val stop = length a
	val newvec = vector_ stop ()
	fun lr j = if j < stop then (update_ newvec j (f(sub_ a j));
				     lr (j+1))
		   else ()
    in lr 0; newvec end

fun findi (p : int * 'a -> bool) (a : 'a vector) : (int * 'a) option =
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

fun mapi (f : int * 'a -> 'b) (a : 'a vector) : 'b vector =
    let val stop = length a
	val newvec = vector_ stop ()
	fun lr j =
	    if j < stop then
		(update_ newvec j (f(j, sub_ a j));
		 lr (j+1))
	    else ()
    in lr 0; newvec end;

fun collate cmp (v1, v2) =
    let val n1 = length v1
	and n2 = length v2
	val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point v1[0..j-1] = v2[0..j-1] *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
		case cmp(sub_ v1 j, sub_ v2 j) of
		    EQUAL => h (j+1)
		  | res   => res
    in h 0 end;
end

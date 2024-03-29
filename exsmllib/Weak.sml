(* Weakarr.sml --- arrays of weak pointers -- 1998-01-08 v. 0.2 *)

structure Weak :> Weak = struct

prim_type 'a weak;

prim_EQtype 'a array
prim_type 'a array_;

prim_val weakarr_ : int -> 'a array_               = 1 "weak_arr"
prim_val weaksub_ : 'a array_ -> int -> 'a         = 2 "weak_sub"
prim_val isdead_  : 'a array_ -> int -> bool       = 2 "weak_isdead"
prim_val update_  : 'a array_ -> int -> 'a -> unit = 3 "set_vect_item"
prim_val length_  : 'a array_ -> int               = 1 "vect_length"
prim_val magic    : 'a -> 'b                       = 1 "identity"

fun from_array (a : 'a  array)  = !(magic a)    : 'a array_
fun make_array (a : '_a array_) = magic (ref a) : 'a array

val maxLen = Architecture.max_array_len;

prim_val weak2arr : 'a weak -> 'a array_ = 1 "identity"
prim_val arr2weak : 'a array_ -> 'a weak = 1 "identity";

fun weak (v : 'a) : 'a weak =
    let val w = weakarr_ 1
    in update_ w 0 v; arr2weak w end

fun get (w : 'a weak) = weaksub_ (weak2arr w) 0

fun set (w : 'a weak, v : 'a) = update_ (weak2arr w) 0 v

fun isweak (w : 'a weak) = isdead_ (weak2arr w) 0;

fun array n =
    if n < 0 orelse n > maxLen then raise Size
    else make_array (weakarr_ n)

fun length a = length_ (from_array a);

fun sub(warr, i) =
    let val a = from_array warr
    in
	if i < 0 orelse i >= length_ a then raise Subscript
	else weaksub_ a i
    end

fun update (warr, i, v) =
    let val a = from_array warr
    in
	if i < 0 orelse i >= length_ a then raise Subscript
	else update_ a i v
    end

fun isdead(warr, i) =
    let val a = from_array warr
    in
	if i < 0 orelse i >= length_ a then raise Subscript
	else isdead_ a i
    end

fun length a = length_ (from_array a);

fun foldl f e a =
    let val a = from_array a
	val stop = length_ a
	fun lr j res =
	    if j < stop then
		if isdead_ a j then lr (j+1) res
		else lr (j+1) (f(weaksub_ a j, res))
	    else res
    in lr 0 e end

fun foldr f e a =
    let val a = from_array a
	fun rl j res =
	    if j >= 0 then
		if isdead_ a j then rl (j-1) res
		else rl (j-1) (f(weaksub_ a j, res))
	    else res
    in rl (length_ a - 1) e end

fun modify f a =
    let val a = from_array a
	val stop = length_ a
	fun lr j =
	    if j < stop then
		if isdead_ a j then lr (j+1)
		else (update_ a j (f(weaksub_ a j)); lr (j+1))
	    else ()
    in lr 0 end

fun app f a =
    let val a = from_array a
	val stop = length_ a
	fun lr j =
	    if j < stop then
		if isdead_ a j then lr (j+1)
		else (f(weaksub_ a j); lr (j+1))
	    else ()
    in lr 0 end

fun sliceend (a, i, NONE) =
        if i<0 orelse i>length a then raise Subscript
	else length a
  | sliceend (a, i, SOME n) =
	if i<0 orelse n<0 orelse i+n>length a then raise Subscript
	else i+n;

fun foldli f e (slice as (a, i, _)) =
    let val a = from_array a
	fun loop stop =
	    let fun lr j res =
		if j < stop then
		    if isdead_ a j then lr (j+1) res
		    else lr (j+1) (f(j, weaksub_ a j, res))
		else res
	    in lr i e end
    in loop (sliceend slice) end;

fun foldri f e (slice as (a, i, _)) =
    let val a = from_array a
	fun loop start =
	    let fun rl j res =
		    if j >= i then
			if isdead_ a j then rl (j-1) res
			else rl (j-1) (f(j, weaksub_ a j, res))
		    else res
	    in rl start e end;
    in loop (sliceend slice - 1) end

fun modifyi f (slice as (a, i, _)) =
    let val a = from_array a
	fun loop stop =
	    let fun lr j =
		if j < stop then
		    if isdead_ a j then lr (j+1)
		    else (update_ a j (f(j, weaksub_ a j)); lr (j+1))
		else ()
	    in lr i end
    in loop (sliceend slice) end;

fun appi f (slice as (a, i, _)) =
    let val a = from_array a
	fun loop stop =
	    let	fun lr j =
		    if j < stop then
			if isdead_ a j then lr (j+1)
			else (f(j, weaksub_ a j); lr (j+1))
		    else ()
	    in lr i end
    in loop (sliceend slice) end;

end

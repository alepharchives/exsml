(* Array -- new basis 1994-11-21, 1995-05-21 *)

structure Array :> Array = struct

(* In fact, type 'a array = 'a array_ ref, but for the static equality
 * type to be right, we need to declare it a prim_EQtype:              *)
prim_EQtype 'a array;
type 'a vector = 'a Vector.vector;

local
    prim_type 'a array_;

    prim_val length_  : 'a array_ -> int               = 1 "vect_length";
    prim_val lengthv_ : 'a vector -> int               = 1 "vect_length";

    prim_val array_  : int -> 'x -> 'a array_          = 2 "make_ref_vect";
    (* array_ has a non-imperative type for the sake of array0, and a
       very flexible type 'x to allow initialization.  Thus type
       correctness inside this unit body depends on type annotations.
    *)

    prim_val vector_ : int -> 'x -> 'a vector          = 2 "make_vect";
    prim_val sub_    : 'a array_ -> int -> 'a          = 2 "get_vect_item";
    prim_val subv_   : 'a vector -> int -> 'a          = 2 "get_vect_item";
    prim_val update_ : 'a array_ -> int -> 'a -> unit  = 3 "set_vect_item";
    prim_val updatev : 'a vector -> int -> 'a -> unit  = 3 "set_vect_item";

    prim_val magic   : 'a -> 'b                        = 1 "identity";

    fun from_array (a : 'a  array)  = !(magic a)    : 'a array_;
    fun make_array (a : '_a array_) = magic (ref a) : 'a array
in

val maxLen = Architecture.max_array_len

fun array(n, v : '_a) =
  if n < 0 orelse n > maxLen then raise Size
  else make_array (array_ n v) : '_a array;

fun tabulate(n, f : int -> '_a) =
  if n < 0 orelse n > maxLen then raise Size else
  let val a = array_ n () : '_a array_
      fun init i = if i >= n then () else (update_ a i (f i); init (i+1))
  in (init 0; make_array a : '_a array) end;

fun fromList (vs : '_a list) =
    let val n = List.length vs
	val a = if n > maxLen then raise Size
		else (array_ n () : '_a array_)
	fun init [] i = ()
	  | init (v::vs) i = (update_ a i v; init vs (i+1))
    in (init vs 0; make_array a : '_a array) end;

fun length a = length_ (from_array a);

fun sub(a, i) =
    let val a = from_array a
    in
	if i < 0 orelse i >= length_ a then raise Subscript
	else sub_ a i
    end

fun update(a, i, v) =
    let val a = from_array a
    in
	if i < 0 orelse i >= length_ a then raise Subscript
	else update_ a i v
    end

fun vector arr =
    Vector.tabulate (length arr, fn i => sub(arr, i))

fun extract (a : 'a array, i, slicelen) =
    let val a = from_array a : 'a array_
	val n = case slicelen of NONE => length_ a - i | SOME n => n
	val newvec = if i<0 orelse n<0 orelse i+n > length_ a
			 then raise Subscript
		     else vector_ n () : 'a vector
	fun copy j =
	    if j<n then
		(updatev newvec j (sub_ a (i+j)); copy (j+1))
	    else
		()
    in copy 0; newvec end;

fun copy {src, dst, di} =
    let
      val array_eq = src = dst
      val src = from_array src
      val dst = from_array dst
    in
      if di < 0 orelse length_ dst < di + (length_ src)
      then
	raise Subscript
      else if array_eq andalso di = 0 (* This is the identity copy *)
      then
	()
      else
	let
	  val stop = length_ src
	  (* The loop function copies one element at a time from left to right *)
	  fun loop i =
	      if i = stop then ()
	      else (update_ dst (di + i) (sub_ src i);
		    loop (i + 1))
	in
	  loop 0
	end
    end

fun copyVec {src, dst, di} =
    let
      val dst = from_array dst
      val src_sz = Vector.length src
    in
      if di < 0 orelse length_ dst < di + src_sz
      then
	raise Subscript
      else
	let
	  fun loop i = if i = src_sz (* Stop criterion *) then ()
		       else (update_ dst (di + i) (Vector.sub (src, i));
			     loop (i + 1))
	in
	  loop 0
	end
    end

fun foldl f e a =
    let val a = from_array a
	val stop = length_ a
	fun lr j res = if j < stop then lr (j+1) (f(sub_ a j, res))
		       else res
    in lr 0 e end

fun foldr f e a =
    let val a = from_array a
	fun rl j res = if j >= 0 then rl (j-1) (f(sub_ a j, res))
		       else res
    in rl (length_ a - 1) e end

fun modify f a =
    let val a = from_array a
	val stop = length_ a
	fun lr j = if j < stop then (update_ a j (f(sub_ a j)); lr (j+1))
		   else ()
    in lr 0 end

fun app f a =
    let val a = from_array a
	val stop = length_ a
	fun lr j = if j < stop then (f(sub_ a j); lr (j+1))
		   else ()
    in lr 0 end

fun sliceend (a, i, NONE) =
        if i<0 orelse i>length a then raise Subscript
	else length a
  | sliceend (a, i, SOME n) =
	if i<0 orelse n<0 orelse i+n>length a then raise Subscript
	else i+n;

fun foldli f e arr =
    let
      val arr = from_array arr
      fun loop stop =
	  let fun lr j res =
		  if j < stop then lr (j+1) (f(j, sub_ arr j, res))
		  else res
	  in lr 0 e end
    in loop (length_ arr) end

fun foldri f e arr =
    let
      val arr = from_array arr
      fun rl j res =
	  if j >= 0 then rl (j-1) (f(j, sub_ arr j, res))
	  else res
    in
      rl (length_ arr - 1) e
    end

fun modifyi f arr =
    let
      val arr = from_array arr
      val stop = length_ arr
      fun lr j = if j < stop then (update_ arr j (f(j, sub_ arr j));
				   lr (j+1))
		 else ()
    in
      lr 0 end

fun appi f arr =
    let
      val arr = from_array arr
      fun loop stop =
	  let fun lr j =
		  if j < stop then (f (j, sub_ arr j); lr (j+1))
		  else ()
	  in lr 0 end
    in
      loop (length_ arr)
    end

fun findi p arr =
    let
      val stop_criterion = length arr
      fun loop i =
	  if i = stop_criterion
	  then
	    NONE
	  else
	    let val element = sub (arr, i)
	    in
	      if p (i, element) then SOME (i, element)
	      else loop (i + 1)
	    end
    in
      loop 0
    end

fun find p arr =
    let
      val stop_criterion = length arr
      fun loop i =
	  if i = stop_criterion then NONE
	  else let val element = sub (arr, i)
	       in
		 if p element then SOME element
		 else loop (i + 1)
	       end
    in
      loop 0
    end

fun exists p arr =
    let
      val stop_criterion = length arr
      fun loop i =
	  if i = stop_criterion then false
	  else
	    p (sub (arr, i)) orelse loop (i + 1)
    in
      loop 0
    end

fun all p arr =
    let
      val stop_criterion = length arr
      fun loop i =
	  if i = stop_criterion then true
	  else
	    p (sub (arr, i)) andalso loop (i + 1)
    in
      loop 0
    end

exception CollateStop of order

fun collate (f : 'a_ * 'a_ -> order) (a1, a2) =
    let
      val a1_sz = length a1
      val a2_sz = length a2
      val stop_criterion = Int.max(a1_sz, a2_sz)
      fun loop i =
            (* First, we check the stop criterion so if the arrays are equal size
	     * we have hit EQUAL all the way through the array, so the arrays must be
	     * equal *)
	    if i = stop_criterion then EQUAL
	    else
	      let
                (* Handle Subscript to fix that the arrays are different sizes *)
		val e1 = sub(a1, i) handle Subscript => raise CollateStop LESS
		val e2 = sub(a2, i) handle Subscript => raise CollateStop GREATER
	      in
		case f (e1, e2) of
		  LESS => LESS
		| GREATER => GREATER
		| EQUAL => loop (i + 1)
	      end
    in
      loop 0 handle CollateStop x => x
    end

end

end
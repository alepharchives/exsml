(* ArraySlice -- SML Basis Library 
   sestoft@dina.kvl.dk 2000-10-18
*)

local
    prim_val magic   : 'a -> 'b                        = 1 "identity";
    type 'a array = 'a Array.array
    type 'a vector = 'a Vector.vector;

    prim_type 'a array_;

    fun from_array (a : 'a  array)  = !(magic a)    : 'a array_;
    fun make_array (a : '_a array_) = magic (ref a) : 'a array
    prim_val sub_    : 'a array_ -> int -> 'a          = 2 "get_vect_item";
    prim_val update_ : 'a array_ -> int -> 'a -> unit  = 3 "set_vect_item";
    prim_val length_  : 'a array_ -> int               = 1 "vect_length";

    prim_val vector_ : int -> 'x -> 'a vector          = 2 "make_vect";
    prim_val subv_   : 'a vector -> int -> 'a          = 2 "get_vect_item";
    prim_val updatev : 'a vector -> int -> 'a -> unit  = 3 "set_vect_item";
in

type 'a slice = 'a array * int * int

(* Invariant on values (a, i, n) of type 'a slice:
 *                  0 <= i <= i+n <= Array.length a, 
 * or equivalently, 0 <= i and 0 <= n and i+n <= Array.length a.  
 *)

fun length (a, i, n) = n;

fun sub((a', i', n'), i) = 
    if i<0 orelse i >= n' then raise Subscript
    else sub_ (from_array a') (i'+i);

fun update ((a', i', n'), i, v)  =
    if i<0 orelse i>=n' then raise Subscript 
    else update_ (from_array a') (i'+i) v;

fun slice (a, i, len) =
    let val alen = Array.length a
    in 
	case len of 
	    NONE   => if 0<=i andalso i<=alen then (a, i, alen - i)
		      else raise Subscript
	  | SOME n => if 0<=i andalso 0<=n andalso n<=alen-i then (a, i, n)
		      else raise Subscript
    end;

fun full a = (a, 0, Array.length a);

fun subslice ((a, i, n), i', NONE) =
    if 0<=i' andalso i'<=n then (a, i+i', n-i')
    else raise Subscript
  | subslice ((a, i, n), i', SOME n') =
    if 0<=i' andalso 0<=n' andalso n'<=n-i' then (a, i+i', n')
    else raise Subscript;
		      
fun base sli = sli;

fun vector (a : 'a array, i, n) = 
    let val a = from_array a : 'a array_ 
	val newvec = vector_ n () : 'a vector
	fun copy j = 
	    if j<n then
		(updatev newvec j (sub_ a (i+j)); copy (j+1))
	    else
		()
    in copy 0; newvec end;

fun copy {src=(a1,i1,n) : 'a slice, dst=a2: 'a array, di=i2} =
    let val a1 = from_array a1
	and a2 = from_array a2
    in
	if i2<0 orelse i2+n > length_ a2 then raise Subscript
	else if i1 < i2 then		(* copy from high to low *)
	         let fun hi2lo j = 
		     if j >= 0 then
			 (update_ a2 (i2+j) (sub_ a1 (i1+j)); hi2lo (j-1))
		     else ()
		 in hi2lo (n-1) end
	     else                       (* i1 >= i2, copy from low to high *)
		 let fun lo2hi j = 
		     if j < n then
			 (update_ a2 (i2+j) (sub_ a1 (i1+j)); lo2hi (j+1))
		     else ()
		 in lo2hi 0 end
    end;

fun copyVec {src : 'a VectorSlice.slice, dst=a2: 'a array, di=i2} =
    let val (a1, i1, n) = VectorSlice.base src
	val a2 = from_array a2
    in
	if i2<0 orelse i2+n > length_ a2 then raise Subscript
	else 
	    let fun lo2hi j = if j < n then
		(update_ a2 (i2+j) (subv_ a1 (i1+j)); lo2hi (j+1))
			      else ()
	    in lo2hi 0 end
    end;

fun isEmpty (_, _, n) = n=0;

fun getItem (a, i, 0) = NONE
  | getItem (a, i, n) = SOME(sub_ (from_array a) i, (a, i+1, n-1));

fun find (p : 'a -> bool) ((a,i,n) : 'a slice) : 'a option = 
    let val a = from_array a
	val stop = i+n
	fun lr j = 
	    if j < stop then 
		if p (sub_ a j) then SOME (sub_ a j) else lr (j+1)
	    else NONE
    in lr i end;

fun exists (p : 'a -> bool) ((a,i,n) : 'a slice) : bool = 
    let val a = from_array a
	val stop = i+n
	fun lr j = j < stop andalso (p (sub_ a j) orelse lr (j+1))
    in lr i end;

fun all (p : 'a -> bool) ((a,i,n) : 'a slice) : bool = 
    let val a = from_array a
	val stop = i+n
	fun lr j = j >= stop orelse (p (sub_ a j) andalso lr (j+1))
    in lr i end;

fun app f (a, i, n) = 
    let val a = from_array a
	val stop = i+n
	fun lr j = if j < stop then (f(sub_ a j); lr (j+1))
		   else ()
    in lr i end;

fun foldl f e (a, i, n) = 
    let val a = from_array a
	val stop = i+n
	fun lr j res = if j < stop then lr (j+1) (f(sub_ a j, res))
		       else res
    in lr i e end;

fun foldr f e (a, i, n) =
    let val a = from_array a
	fun rl j res = if j >= i then rl (j-1) (f(sub_ a j, res))
		       else res
    in rl (i+n-1) e end;

fun modify f (a, i, n) = 
    let val a = from_array a
	val stop = i+n
	fun lr j = if j < stop then (update_ a j (f(sub_ a j)); lr (j+1))
		   else ()
    in lr i end;

fun findi (p : int * 'a -> bool) ((a,i,n) : 'a slice) : (int * 'a) option = 
    let val a = from_array a
	val stop = i+n
	fun lr j = 
	    if j < stop then 
		if p (j, sub_ a j) then SOME (j, sub_ a j) else lr (j+1)
	    else NONE
    in lr i end;

fun appi f (a, i, n) = 
    let val a = from_array a
	val stop = i+n
	fun lr j = 
	    if j < stop then (f(j, sub_ a j); lr (j+1)) 
	    else ()
    in lr i end;

fun foldli f e (a, i, n) = 
    let val a = from_array a
	val stop = i+n
	fun lr j res = 
	    if j < stop then lr (j+1) (f(j, sub_ a j, res))
	    else res
    in lr i e end;

fun foldri f e (a, i, n) = 
    let val a = from_array a
	fun rl j res = 
	    if j >= i then rl (j-1) (f(j, sub_ a j, res))
	    else res
    in rl (i+n-1) e end;

fun modifyi f (a, i, n) = 
    let val a = from_array a
	val stop = i+n
	fun lr j = 
	    if j < stop then (update_ a j (f(j, sub_ a j)); lr (j+1))
	    else ()
    in lr i end;

fun collate cmp ((a1,i1,n1), (a2,i2,n2)) =
    let val a1 = from_array a1
	and a2 = from_array a2
	val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point a1[i1..i1+j-1] = a2[i2..i2+j-1] *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
		case cmp(sub_ a1 (i1+j), sub_ a2 (i2+j)) of
		    EQUAL => h (j+1)
		  | res   => res
    in h 0 end;

end
structure ArraySlice :> ArraySlice =
struct

type 'a slice = { arr: 'a Array.array,
		  index: int,
		  len: int }

fun length {arr, index, len} = len

fun in_bounds {arr, index, len} k =
    k >= 0 andalso k < len

fun sub (slc as {arr, index, len}, k) =
    if in_bounds slc k then Array.sub (arr, index+k)
    else raise Subscript

fun update (slc as {arr, index, len}, k, v) =
    if in_bounds slc k then Array.update (arr, index+k, v)
    else raise Subscript

fun slice (arr, start, sz) =
    let
      val arr_sz = Array.length arr
    in
      case sz of
	NONE => if start < 0 orelse arr_sz < start then raise Subscript
		else
		  {arr = arr, index = start, len = arr_sz - start}
      | SOME l =>
	  if start < 0 orelse l < 0 orelse arr_sz < start + l
	  then raise Subscript
	  else
	    {arr = arr, index = start, len = l}
end

fun full arr = slice(arr, 0, NONE)

fun subslice (slc as {arr, index, len}, start, sz) =
    case sz of
      NONE => if start < 0 orelse len < start then raise Subscript
	      else
		{arr = arr, index = start + index, len = len - start}
    | SOME l =>
        if start < 0 orelse l < 0 orelse len < start + l
	then raise Subscript
	else
	  {arr = arr, index = start + index, len = l}

fun base {arr, index, len} = (arr, index, len)
fun vector {arr, index, len} =
    Vector.tabulate(len, (fn j => Array.sub(arr, index+j)))

fun isEmpty {arr, index, len} = len = 0
fun getItem {arr, index, len} =
    if len = 0 then NONE
    else SOME (Array.sub (arr, index), {arr = arr,
					index = index+1,
					len = len-1})

fun app f {arr, index, len} =
    let
      fun lr j = if j < len then (f(Array.sub (arr, index+j));
				  lr (j+1))
		 else ()
    in
      lr 0
    end

fun appi f {arr, index, len} =
    let
      fun lr j = if j < len then (f(j, Array.sub (arr, index+j));
				  lr (j+1))
		 else ()
    in lr 0 end

fun copy {src = {arr, index, len}, dst, di} =
    (* Bounds check *)
    if di < 0 orelse (Array.length dst) < di + len then raise Subscript
    (* Overlap check. If the pieces overlap we copy from right to left,
     * otherwise we copy from left to right *)
    else if arr = dst andalso index < di andalso di <= index+len
    then let
	fun rl j = if j >= 0 then (Array.update(dst, di+j,
						Array.sub(arr, index+j));
				   rl (j-1))
		   else ()
      in
	rl (len-1)
      end
    else
      let
	fun lr j = if j < len then (Array.update(dst, di+j,
						 Array.sub(arr, index+j));
				    lr (j+1))
		   else ()
      in
	lr 0
      end

fun find p {arr, index, len} =
    let
      fun lr j = if j < len then
		   let val elem = Array.sub(arr, index+j)
		   in
		     if p elem then SOME elem else lr (j+1)
		   end
		 else
		   NONE
    in
      lr 0
    end

fun findi p {arr, index, len} =
    let
      fun lr j = if j < len then
		   let val elem = Array.sub(arr, index+j)
		   in
		     if p (j, elem) then SOME (j, elem) else lr (j+1)
		   end
		 else
		   NONE
    in
      lr 0
    end

fun exists p {arr, index, len} =
    let
      fun lr j = if j < len then
		   p (Array.sub(arr, index+j)) orelse lr (j+1)
		 else false
    in lr 0 end

fun all p {arr, index, len} =
    let
      fun lr j = if j < len then
		   p (Array.sub(arr, index+j)) andalso lr (j+1)
		 else true
    in lr 0 end


fun foldl f e {arr, index, len} =
    let
      fun lr j res = if j < len then
		       lr (j+1) (f (Array.sub(arr, index+j), res))
		     else res
    in lr 0 e end

fun foldli f e {arr, index, len} =
    let
      fun lr j res = if j < len then
		       lr (j+1) (f (j, Array.sub(arr, index+j), res))
		     else
		       res
    in lr 0 e end

fun foldr f e {arr, index, len} =
    let
      fun rl j res = if j >= 0 then
		       rl (j-1) (f (Array.sub(arr, index+j), res))
		     else res
    in
     rl len e
    end

fun foldri f e {arr, index, len} =
    let
      fun rl j res = if j >= 0 then
		       rl (j-1) (f (j, Array.sub(arr, index+j), res))
		     else res
    in
      rl len e
    end

fun modify f {arr, index, len} =
    let
      fun lr j = if j < len then
		   (Array.update (arr, index+j, f(Array.sub(arr, index+j)));
		    lr (j+1))
		 else ()
    in lr 0 end


fun modifyi f {arr, index, len} =
    let
      fun lr j = if j < len then
		   (Array.update (arr, index+j, f(j, Array.sub(arr, index+j)));
		    lr (j+1))
		 else ()
    in lr 0 end

fun collate f (slc1, slc2) =
    case (getItem slc1, getItem slc2) of
      (NONE, SOME _) => LESS
    | (SOME _, NONE) => GREATER
    | (NONE, NONE) => EQUAL
    | (SOME (e1, r1), SOME (e2, r2)) =>
        (case f (e1, e2) of
	   LESS => LESS
	 | GREATER => GREATER
	 | EQUAL => collate f (r1, r2))

end

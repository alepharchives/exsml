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

(* ArraySlice -- SML Basis Library *)

type 'a slice

val length   : 'a slice -> int
val sub      : 'a slice * int -> 'a
val update   : 'a slice * int * 'a  -> unit
val slice    : 'a Array.array * int * int option -> 'a slice
val full     : 'a Array.array -> 'a slice
val subslice : 'a slice * int * int option -> 'a slice
val base     : 'a slice -> 'a Array.array * int * int
val vector   : 'a slice -> 'a Vector.vector
val copy     : {src: 'a slice, dst: 'a Array.array, di: int} -> unit
val copyVec  : {src: 'a VectorSlice.slice, dst: 'a Array.array, di: int} 
               -> unit 
val isEmpty  : 'a slice -> bool
val getItem  : 'a slice -> ('a * 'a slice) option

val find     : ('a -> bool) -> 'a slice -> 'a option
val exists   : ('a -> bool) -> 'a slice -> bool
val all      : ('a -> bool) -> 'a slice -> bool

val app      : ('a -> unit) -> 'a slice -> unit
val foldl    : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
val foldr    : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
val modify   : ('a -> 'a) -> 'a slice -> unit

val findi    : (int * 'a -> bool) -> 'a slice -> (int * 'a) option
val appi     : (int * 'a -> unit) -> 'a slice -> unit
val foldli   : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
val foldri   : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
val modifyi  : (int * 'a -> 'a) -> 'a slice -> unit

val collate  : ('a * 'a -> order) -> 'a slice * 'a slice -> order

(* 
   ['ty slice] is the type of array slices, that is, sub-arrays.  
   The slice (a,i,n) is valid if 0 <= i <= i+n <= size s, 
                or equivalently, 0 <= i and 0 <= n and i+n <= size s.  
   A valid slice sli = (a,i,n) represents the sub-array a[i...i+n-1],
   so the elements of sli are a[i], a[i+1], ..., a[i+n-1], and n is
   the length of the slice.  Only valid slices can be constructed by
   the functions below.

   [length sli] returns the number n of elements in sli = (s,i,n).

   [sub (sli, k)] returns the k'th element of the slice, that is,
   a(i+k) where sli = (a,i,n).  Raises Subscript if k<0 or k>=n.

   [update (sli, k, x)] destructively replaces the k'th element of sli
   by x.  That is, replaces a(k+i) by x, where sli = (a,i,n).  Raises
   Subscript if i<0 or i>=n.

   [slice (a, i, NONE)] creates the slice (a, i, length a-i),
   consisting of the tail of a starting at i.  
   Raises Subscript if i<0 or i > Array.length a.  
   Equivalent to slice (a, i, SOME(Array.length a - i)).

   [slice (a, i, SOME n)] creates the slice (a, i, n), consisting of
   the sub-array of a with length n starting at i.  Raises Subscript
   if i<0 or n<0 or i+n > Array.length a.  

       slice             meaning 
       ----------------------------------------------------------
       (a, 0, NONE)      the whole array              a[0..len-1]   
       (a, 0, SOME n)    a left sub-array (prefix)    a[0..n-1]
       (a, i, NONE)      a right sub-array (suffix)   a[i..len-1]
       (a, i, SOME n)    a general slice              a[i..i+n-1] 

   [full a] creates the slice (a, 0, length a).  
   Equivalent to slice(a,0,NONE)

   [subslice (sli, i', NONE)] returns the slice (a, i+i', n-i') when
   sli = (a,i,n).  Raises Subscript if i' < 0 or i' > n.

   [subslice (sli, i', SOME n')] returns the slice (a, i+i', n') when
   sli = (a,i,n).  Raises Subscript if i' < 0 or n' < 0 or i'+n' > n.

   [base sli] is the concrete triple (a, i, n) when sli = (a, i, n).

   [vector sli] creates and returns a vector consisting of the
   elements of the slice, that is, a[i..i+n-1] when sli = (a,i,n).

   [copy {src, dst, di}] copies the elements of slice src = (a,i,n),
   that is, a[i..i+n-1], to the destination segment dst[di..di+n-1].
   Raises Subscript if di<0 or if di+n > length dst.  Works also if
   the array underlying sli is the same as dst, and the slice overlaps
   with the destination segment.

   [copyVec {src, dst, di}] copies the elements of the vector slice
   src = (v,i,n), that is, v[i..i+n-1], to dst[di..di+n-1].  Raises
   Subscript if di<0, or if len=NONE and di + n > length dst.  

   [isEmpty sli] returns true if the slice sli = (a,i,n) is empty,
   that is, if n=0.

   [getItem sli] returns SOME(x, rst) where x is the first element and
   rst the remainder of sli, if sli is non-empty; otherwise returns
   NONE.  

   [find p sli] applies p to each element x of sli, from left to
   right, until p(x) evaluates to true; returns SOME x if such an x
   exists, otherwise NONE.

   [exists p sli] applies p to each element x of sli, from left to right,
   until p(x) evaluates to true; returns true if such an x exists,
   otherwise false.

   [all p sli] applies p to each element x of sli, from left to right,
   until p(x) evaluates to false; returns false if such an x exists,
   otherwise true.

   [app f sli] applies f to all elements of sli = (a,i,n), from
   left to right.  That is, applies f to a[j+i] for j=0,1,...,n.

   [foldl f e sli] folds function f over sli = (a,i,n) from left to right.  
   That is, computes f(a[i+n-1], f(a[i+n-2],..., f(a[i+1], f(a[i], e))...)).

   [foldr f e sli] folds function f over sli = (a,i,n) from right to left.  
   That is, computes f(a[i], f(a[i+1],..., f(a[i+n-2], f(a[i+n-1], e))...)).

   [modify f sli] modifies the elements of the slice sli = (a,i,n) by
   function f.  That is, applies f to a[i+j] and updates a[i+j] with
   the result f(a[i+j]) for j=0,1,...,n.

   The following iterators generalize the above ones by also passing
   the index into the array a underlying the slice to the function
   being iterated.

   [findi p sli] applies p to the elements of sli = (a,i,n) and the
   underlying array indices, and returns the least (j, a[j]) for which
   p(j, a[j]) evaluates to true, if any; otherwise returns NONE.  That
   is, evaluates p(j, a[j]) for j=i,..i+n-1 until it evaluates to true
   for some j, then returns SOME(j, a[j]); otherwise returns NONE.

   [appi f sli] applies f to the slice sli = (a,i,n) and the
   underlying array indices.  That is, applies f to successive pairs
   (j, a[j]) for j=i,i+1,...,i+n-1.

   [foldli f e sli] folds function f over the slice sli = (a,i,n) and
   the underlying array indices from left to right.  That is, computes 
   f(i+n-1, a[i+n-1], f(..., f(i+1, a[i+1], f(i, a[i], e)) ...)).  

   [foldri f e sli] folds function f over the slice sli = (a,i,n) and
   the underlying array indices from right to left.  That is, computes
   f(i, a[i], f(i+1, a[i+1], ..., f(i+n-1, a[i+n-1], e) ...)).

   [modifyi f sli] modifies the elements of the slice sli = (a,i,n) by
   applying function f to the slice elements and the underlying array
   indices.  That is, applies f to (j, a[j]) and updates a[j] with the
   result f(j, a[j]) for j=i,i+1,...,i+n-1.  
   
   [collate cmp (sli1, sli2)] returns LESS, EQUAL or GREATER according
   as sli1 precedes, equals or follows sli2 in the lexicographic
   ordering on slices induced by the ordering cmp on elements.
*)
signature ArraySlice =
sig

type 'a slice
val length : 'a slice -> int
val sub : 'a slice * int -> 'a
val update : 'a slice * int * 'a -> unit
val slice : 'a Array.array * int * int option -> 'a slice
val full : 'a Array.array -> 'a slice
val subslice : 'a slice * int * int option -> 'a slice
val base : 'a slice -> 'a Array.array * int * int
val vector : 'a slice -> 'a Vector.vector

val copy : { src : 'a slice,
	     dst : 'a Array.array,
	     di : int } -> unit

(* Needs vectorslices
val copyVec : { src : 'a Vectorslice.slice,
		dst : 'a Array.array,
		di : int } -> unit
*)

val isEmpty : 'a slice -> bool
val getItem : 'a slice -> ('a * 'a slice) option
val app : ('a -> unit) -> 'a slice -> unit
val appi : (int * 'a -> unit) -> 'a slice -> unit

val find  : ('a -> bool) -> 'a slice -> 'a option
val findi : (int * 'a -> bool)
              -> 'a slice -> (int * 'a) option

val exists : ('a -> bool) -> 'a slice -> bool
val all : ('a -> bool) -> 'a slice -> bool

val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
val foldl  : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
val foldr  : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b


val modifyi : (int * 'a -> 'a) -> 'a slice -> unit
val modify  : ('a -> 'a) -> 'a slice -> unit

val collate : ('a * 'a -> order)
                -> 'a slice * 'a slice -> order

end

(*
   The ArraySlice structure provides an abstraction of subarrays for
   polymorphic arrays. A slice value can be viewed as a triple (a, i,
   n), where a is the underlying array, i is the starting index, and n
   is the length of the subarray, with the constraint that 0 <= i <= i
   + n <= |a|. Slices provide a convenient notation for specifying and
   operating on a contiguous subset of elements in an array.

   ['a slice] the type of array slices.

   [length slc] returns the length of a slice

   [sub slc k] returns the k-th element of slc. Raises Subscript on
   out of bounds

   [update slc k v] update the k-th element of slc to be v. Raises
   Subscript on out of bounds.

   [slice (arr, i, sz)] create a new slice from array starting at
   element i with a size of sz. If sz is NONE the end of the array is
   taken; if SOME s then s elements are taken.

   [full arr] equivalent to calling slice(arr, 0, NONE).

   [subslice (slc, i, n)] create a slice of a slice, ie a subclie of
   the slice slc at index i and length n. Raises Subscript if this is
   out of bounds.

   [base slc] returns the underlying triple (arr, i, n) for the slice.

   [vector slc] convert a slice to a vector (copies)

   [copy {src, dst, di }] copy the slice src into the array dst
   beginning from index di in the destination array. Implementation
   note: Correctly handles overlap and raises subscript on out of
   bounds.

   [copyVec {src, dst, di}] like copy for for vectorslices.

   [isEmpty slc] predicate; is the slice empty?

   [getItem slc] slice traverser. Returns the element at index 0 and a
   new (one element shorter) slice where the first element has been
   removed.

   [appi f slc]

   [app f slc] apply the function f on each element in the slice. The
   appi variant also supplies the index to the function f.

   [modifyi f slc]
   [modify f slc] modify the slice in increasing index order by
   invoking the function f on each element and replacing the contents
   in the slice with the result of the function f. The modifyi variant
   also supplies the index to f.


   [foldli f e slc]
   [foldl f e slc] perform a foldl over the slice slc. The foldli
   variant also supplies the index to the function f.

   [foldri f e slc]
   [foldr f e slc] perform a foldr over the slice slc. The foldri
   variant also supplies the index to the function f.

   [findi p slc]
   [find p slc] searches the slice in increasing index order. The
   first element to satisfy the predicate p is returned. The findi
   variant of this function also supplies the index in the slice to
   the predicate function.

   [exists p slc] searches the slice in increasing order. Returns true
   if there exists an element in slc satisfying p; false otherwise.

   [all p slc] search the slice in increasing order. Return true if
   all elements in slc satisfies the predicate p; false otherwise.

   Implementation note: exists and all are short-circuiting.

   [collate f (slc1, slc2)] use the ordering function f to do
   lexicographic ordering on the slices slc1 and slc2

 *)

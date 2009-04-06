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

val modifyi : (int * 'a -> 'a) -> 'a slice -> unit
val modify  : ('a -> 'a) -> 'a slice -> unit

(*



val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b

val foldr  : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b

val collate : ('a * 'a -> order)
                -> 'a slice * 'a slice -> order
*)
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

 *)

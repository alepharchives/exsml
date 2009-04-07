(* Array -- SML Basis Library *)

prim_EQtype 'a array
type 'a vector = 'a Vector.vector

val maxLen   : int

val array    : int * '_a -> '_a array
val tabulate : int * (int -> '_a) -> '_a array
val fromList : '_a list -> '_a array

val length   : 'a array -> int
val sub      : 'a array * int -> 'a
val update   : 'a array * int * 'a  -> unit
(* extract is not 2003 basis *)
val extract  : 'a array * int * int option -> 'a Vector.vector

val vector   : 'a array -> 'a vector

val copy     : {src: 'a array, dst: 'a array, di: int} -> unit
val copyVec  : {src: 'a vector, dst: 'a array, di: int} -> unit

val app      : ('a -> unit) -> 'a array -> unit
val foldl    : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
val foldr    : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
val modify   : ('a -> 'a) -> 'a array -> unit

val appi     : (int * 'a -> unit) -> 'a array -> unit
val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
val modifyi : (int * 'a -> 'a) -> 'a array -> unit

val findi    : (int * 'a -> bool) -> 'a array -> (int * 'a) option
val find     : ('a -> bool) -> 'a array -> 'a option
val exists   : ('a -> bool) -> 'a array -> bool
val all      : ('a -> bool) -> 'a array -> bool
val collate  : ('a * 'a -> order) -> 'a array * 'a array -> order

(*
   ['ty array] is the type of one-dimensional, mutable, zero-based
   constant-time-access arrays with elements of type 'ty.  Type
   'ty array admits equality even if 'ty does not.  Arrays a1 and a2
   are equal if both were created by the same call to a primitive
   (array, tabulate, fromList).

   [maxLen] is the maximal number of elements in an array.

   [array(n, x)] returns a new array of length n whose elements are all x.
   Raises Size if n<0 or n>maxLen.

   [vector arr] convert the array arr to a vector.

   [tabulate(n, f)] returns a new array of length n whose elements
   are f 0, f 1, ..., f (n-1), created from left to right.  Raises
   Size if n<0 or n>maxLen.

   [fromList xs] returns an array whose elements are those of xs.
   Raises Size if length xs > maxLen.

   [length a] returns the number of elements in a.

   [sub(a, i)] returns the i'th element of a, counting from 0.
   Raises Subscript if i<0 or i>=length a.  To make `sub' infix, use
   the declaration
                             infix 9 sub

   [update(a, i, x)] destructively replaces the i'th element of a by x.
   Raises Subscript if i<0 or i>=length a.

   [extract(a, i, NONE)] returns a vector of the elements a[i..length a-1]
   of a.  Raises Subscript if i<0 or i>length a.

   [extract(a, i, SOME len)] returns a vector of the elements a[i..i+len-1]
   of a.  Raises Subscript if i<0 or len<0 or i+len>length a or
   len>Vector.maxLen.

   [copy {src, dst, di}]
   [copyVec {src, dst, di}] These functions copy the entire array or
   vector src into the array dst, with the i(th) element in src, for 0
   <= i < |src|, being copied to position di + i in the destination
   array. If di < 0 or if |dst| < di+|src|, then the Subscript
   exception is raised.

   Implementation note: In copy, if dst and src are equal, we must
   have di = 0 to avoid an exception, and copy is then the identity.

   [foldl f e a] folds function f over a from left to right.  That is,
   computes f(a[len-1], f(a[len-2], ..., f(a[1], f(a[0], e)) ...)),
   where len is the length of a.

   [foldr f e a] folds function f over a from right to left.  That is,
   computes f(a[0], f(a[1], ..., f(a[len-2], f(a[len-1], e)) ...)),
   where len is the length of a.

   [app f a] applies f to a[j] for j=0,1,...,length a-1.

   [modify f a] applies f to a[j] and updates a[j] with the result
   f(a[j]) for j=0,1,...,length a-1.

   The following iterators generalize the above ones as they also pass
   in the current index:

   [foldli f e arr]
   [foldri f e arr]
   [modifyi f arr]
   [appi f arr]

   [find f arr]
   [findi f arr] These functions apply f to each element of the array
   arr, from left to right (i.e., increasing indices), until a true
   value is returned. If this occurs, the functions return the
   element; otherwise, they return NONE. The more general version
   findi also supplies f with the array index of the element and, upon
   finding an entry satisfying the predicate, returns that index with
   the element.

   [exists p arr] apply p on each element x in the array in increasing
   order. If (p x) evaluates to true for any element then the exists
   function returns true; otherwise it returns false.

   [all p arr] apply p on each element x in the array in increasing
   order. Returns true if all elements satisfy the predicate; false
   otherwise.

   [collate f (a1, a2)] performs lexicographic comparison of the two
   arrays using the given ordering f on elements.

*)

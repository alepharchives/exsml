(* ListPair -- SML Basis Library *)

exception UnequalLengths

val zip    : 'a list * 'b list -> ('a * 'b) list
val zipEq : 'a list * 'b list -> ('a * 'b) list

val unzip  : ('a * 'b) list -> 'a list * 'b list

val app   : ('a * 'b -> unit) -> 'a list * 'b list -> unit
val appEq : ('a * 'b -> unit) -> 'a list * 'b list -> unit

val map   : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
val mapEq : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
val foldl   : ('a * 'b * 'c -> 'c)
                -> 'c -> 'a list * 'b list -> 'c
val foldr   : ('a * 'b * 'c -> 'c)
                -> 'c -> 'a list * 'b list -> 'c
val foldlEq : ('a * 'b * 'c -> 'c)
                -> 'c -> 'a list * 'b list -> 'c
val foldrEq : ('a * 'b * 'c -> 'c)
                -> 'c -> 'a list * 'b list -> 'c
val all    : ('a * 'b -> bool) -> 'a list * 'b list -> bool
val exists : ('a * 'b -> bool) -> 'a list * 'b list -> bool
val allEq : ('a * 'b -> bool) -> 'a list * 'b list -> bool
(*
   These functions process pairs of lists.  No exception is raised
   when the lists are found to be of unequal length.  Instead the
   excess elements from the longer list are disregarded.

   [exception UnequalLenghts] This exception is raised by those
   functions that require arguments of identical length.

   [zipEq (xs, ys)]
   [zip (xs, ys)] returns the list of pairs of corresponding elements
   from xs and ys. f the lists are of unequal lengths, zip ignores the
   excess elements from the tail of the longer one, while zipEq raises
   the exception UnequalLengths.

   [unzip xys] returns a pair (xs, ys), where xs is the list of first
   components of xys, and ys is the list of second components from
   xys.  Hence zip (unzip xys) has the same result and effect as xys.

   [appEq f (xs, ys)]
   [app f (xs, ys)] applies function f to the pairs of corresponding
   elements of xs and ys and returns ().  Hence app f (xs, ys) has the
   same result and effect as List.app f (zip (xs, ys)). the appEq
   variant raises UnequalLengths if the lengths mismatches while the
   app variant does not.

   Implementation note: The appEq does not test the length before
   applying f to the first bunch of pair elements. That is, the
   side-effect of f is executed until it is found out that the list
   differs in size; then the exception is raised. The implementation
   is the one specified en the 2003 basis library.

   [mapEq f (xs, ys)]
   [map f (xs, ys)] applies function f to the pairs of corresponding
   elements of xs and ys and returns the list of results.  Hence map f
   (xs, ys) has the same result and effect as List.map f (zip (xs,
   ys)). The -Eq variant raises UnequalLengths if the size of the
   lists does not agree.

   [foldr f e (xs, ys)] evaluates f(x1, y1, f(x2, y2, f(..., f(xn, yn, e))))
   where xs = [x1, x2, ..., x(n-1), xn, ...],
         ys = [y1, y2, ..., y(n-1), yn, ...],
   and    n = min(length xs, length ys).
   Equivalent to List.foldr (fn ((x, y), r) => f(x, y, r)) e (zip(xs, ys)).

   [foldl f e (xs, ys)] evaluates f(xn, yn, f( ..., f(x2, y2, f(x1, y1, e))))
   where xs = [x1, x2, ..., x(n-1), xn, ...],
         ys = [y1, y2, ..., y(n-1), yn, ...],
   and    n = min(length xs, length ys).
   Equivalent to List.foldl (fn ((x, y), r) => f(x, y, r)) e (zip(xs, ys)).

   [foldrEq f e (xs, ys)]
   [foldlEq f e (xs, ys)] Variants that raise UnequalLengths if the
   lengths does not match.

   [allEq p (xs, ys)]
   [all p (xs, ys)] applies predicate p to the pairs of corresponding
   elements of xs and ys until p evaluates to false or one or both
   lists is exhausted; returns true if p is true of all such pairs;
   otherwise false.  Hence all p (xs, ys) has the same result and
   effect as List.all p (zip (xs, ys)). The allEq variant raises
   UnequalLenghts if the lenght of xs and ys are not the same.

   [exists p (xs, ys)] applies predicate p to the pairs of corresponding
   elements of xs and ys until p evaluates to true or one or both
   lists is exhausted; returns true if p is true of any such pair;
   otherwise false.  Hence exists p (xs, ys) has the same result and
   effect as List.exists p (zip (xs, ys)).

*)

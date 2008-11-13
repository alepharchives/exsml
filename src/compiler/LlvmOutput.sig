(* Human readable output of LLVM IR code *)

(* There is a type for outputs, we call it t *)
type t

val intersperse : t -> t list -> t
(*
    separate a list of t's with a t. Ie, push a delimiter in between
    a list of already output blocks.
*)

val sorround : t -> t -> t -> t
(*
    sorround left right t will return left followed by t followed by
    right. It is used for the implementation of the following bits
*)

val parens : t -> t
(* Sorround some output with parenthesis, '(', ')' *)

val braces : t -> t
(* Sorround some output with braces, '{', '}' *)

val brackets : t -> t
(* Sorround some output with brackets, '[', ']' *)

val seq_space : t list -> t
(* Print a sequence of t's with space between them *)

val seq : t list -> t
(* Print a sequence of t's with a '\n' between them *)

val commas : t list -> t
(* Print a sequence of t's with a ', ' between them *)

val str : string -> t
(* Make a string into a t *)

val integer : int -> t
(* Make an integer into a t *)

val real : real -> t
(* Make a real into a t *)

val conc : t list -> t
(* Concatenate a list of t's *)

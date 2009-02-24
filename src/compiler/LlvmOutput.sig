(*
    MOSML-LLVM - LLVM bindings for Moscow ML
    Copyright (C) 2008,2009 - Jesper Louis Andersen

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*)

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

val vector : t -> t
(* Sorround some output with vector elements, '<', '>' *)

val seq_space : t list -> t
(* Print a sequence of t's with space between them *)

val seq : t list -> t
(* Print a sequence of t's *)

val lines : t list -> t
(* Intersperse with a '\n' *)

val commas : t list -> t
(* Print a sequence of t's with a ', ' between them *)

val str : string -> t
(* Make a string into a t *)

val quoted_str : string -> t
(* Make a string foobarbaz into a quoted "foobarbaz" string *)

val integer : int -> t
(* Make an integer into a t *)

val real : real -> t
(* Make a real into a t *)

val conc : t list -> t
(* Concatenate a list of t's *)

val null : t
(* Null value *)

val to_string : t -> string
(* Convert a output to a string *)

val to_stream : t -> TextIO.outstream -> unit
(* Output to a stream *)

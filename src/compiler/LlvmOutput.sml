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

(* Implementation of Llvm output structure *)
exception LlvmOutputICE

(* Datatype for output. We use an intermediate type for outputting since it
 * is a lot easier to work with. Outputting goes from Llvm to this intermediate
 * type and then to real output.
 *)
datatype t = O_Str of string
	   | O_Seq of t list
	   | O_Int of int (* FIXME: 32 bit *)
	   | O_Real of real
	   | O_Conc of t * t (* Redundant, O_Conc (a,b) == O_Seq [a, b] *)
	   | O_Null (* Redundant, exists in O_Seq [] *)

(* Simplification of the intermediate structure. Certain intermediate constructs
 * are transformed down to a more canonical form so there is less to work with.
 *)
fun simplify t =
    case t of
      O_Seq ts => O_Seq (List.map simplify ts)
    | O_Conc (a, b) => O_Seq [simplify a, simplify b]
    | O_Null => O_Seq []
    (* Primitives are handled with one case *)
    | x => x

(* Simple output to a string by folding the datatype into one *)
fun to_string t =
    case simplify t of
      O_Str s => s
    | O_Seq ks => String.concat (List.map to_string ks)
    | O_Int i => Int.toString i
    | O_Real r => Real.toString r
    | _ => raise LlvmOutputICE

(* Output to a stream.
 * TODO: Improve the performance of this function
 *)
fun to_stream t outstream =
    let val str = to_string t
    in  TextIO.output (outstream, str)
    end

(* We use O_Seq [] to play the role of a 'Null' element in type t *)
fun intersperse delimiter ts =
    let fun f [] = O_Null
	  | f [x] = x
	  | f (x :: xs) = (case f xs of
			     O_Seq r => O_Seq (x :: delimiter :: r)
			   | O_Null => O_Seq []
			   | _ => raise LlvmOutputICE)
    in
      f ts
    end



fun str x = O_Str x
fun integer i = O_Int i
fun real r = O_Real r
val null = O_Null

fun commas x = intersperse (str ", ") x

fun sorround left right elems =
    case elems of
      O_Seq es => O_Seq [left, intersperse (str ", ") es, right]
    | x => O_Seq [left, x, right]

fun parens elems = sorround (str "(") (str ")") elems
fun braces elems = sorround (str "{") (str "}") elems
fun brackets elems = sorround (str "[") (str "]") elems

(* LLVM uses a rather special vector designation *)
fun vector elems = sorround (str "<") (str ">") elems

fun seq_space es = intersperse (str " ") es
fun seq es = intersperse (str "\n") es
fun conc es = O_Seq es



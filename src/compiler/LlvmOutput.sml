(* Implementation of Llvm output structure *)
exception LlvmOutputICE

datatype t = O_Str of string
	   | O_Seq of t list
	   | O_Int of int (* FIXME: 32 bit *)
	   | O_Real of real
	   | O_Conc of t * t (* Redundant, O_Conc (a,b) == O_Seq [a, b] *)
	   | O_Null (* Redundant, exists in O_Seq [] *)

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
    | _ => raise LlvmOutputICE

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

fun commas x = intersperse (str ", ") x

fun sorround left right elems =
    case elems of
      O_Seq es => O_Seq [left, intersperse (str ", ") es, right]
    | x => O_Seq [left, x, right]

fun parens elems = sorround (str "(") (str ")") elems
fun braces elems = sorround (str "{") (str "}") elems
fun brackets elems = sorround (str "[") (str "]") elems

fun seq_space es = intersperse (str " ") es
fun seq es = intersperse (str "\n") es
fun conc es = O_Seq es



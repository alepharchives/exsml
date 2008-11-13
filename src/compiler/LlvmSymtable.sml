(* Implementation of Symbol Tables for the LLVM backend *)

exception NotFound

type (''a, 'b) t = (''a * 'b) list

val empty = []

fun enter k v t = (k, v) :: t

fun find k t =
    case List.find (fn (x, _) => x = k) t of
      NONE => raise NotFound
    |  SOME (_, v) => v

fun lookup k t =
    case List.find (fn (x, _) => x = k) t of
      NONE => NONE
    | SOME (_, v) => SOME v


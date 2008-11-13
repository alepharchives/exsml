(*
 This is the signature of Symbol Tables used in the LLVM backend of harmless
 *)

exception NotFound
(* This exception is raised when a symbol table search fails *)

type (''a, 'b) t
(*
 This is a symbol table mapping keys (of type ''a) to values (of type 'b). Note
 we are currently requiring the keys to be eqtypes, but later on we will probably
 require an ordering on the keys instead through a functor.
 *)

val empty : (''a, 'b) t
(* This is the empty symbol table *)

val enter : ''a -> 'b -> (''a, 'b) t -> (''a, 'b) t
(* Enter a new pair into the symbol table *)

val find : ''a -> (''a, 'b) t -> 'b
(* Lookup a key in a symbol table, raises NotFound if it is not there *)

val lookup : ''a -> (''a, 'b) t -> 'b option
(* Lookup variant that returns an option type rather than an exception raise *)

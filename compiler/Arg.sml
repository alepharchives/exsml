(* arg.sml *)

open BasicIO Fnlib;

exception Bad of string

datatype spec =
    String  of (string -> unit)
  | Int     of (int -> unit)
  | Unit    of (unit -> unit)
  | Real    of (real -> unit)

datatype error =
    Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

fun stop error =
  let val progname = CommandLine.name ()
      val message =
        case error of
            Unknown s =>
              progname ^ ": unknown option: \"" ^ s ^ "\"."
          | Missing s
              => progname ^ ": option \"" ^ s ^ "\" needs an argument."
          | Wrong (opt, arg, expected)
              => progname ^ ": wrong argument \"" ^ arg ^ "\"; option \""
                   ^ opt ^ "\" expects " ^ expected ^ "."
          | Message s
              => progname ^ ": " ^ s
  in
     output(std_err, message); output(std_err, "\n"); flush_out std_err;
     exit 2
  end;

fun parse speclist anonfun =
  let fun p [] = ()
        | p (s::t) =
            if size s >= 1 andalso CharVector.sub(s, 0) = #"-"
            then doKey s t
            else ((anonfun s; p t)
                   handle Bad m => stop (Message m))
      and doKey s l =
          let val action =
		  lookup s speclist
                    handle Subscript => stop (Unknown s)
          in
            (case (action, l) of
               (Unit f, l) => (f (); p l)
             | (String f, arg::t) => (f arg; p t)
             | (Int f, arg::t) =>
               let val arg_i =
		       case Int.fromString arg of
			 NONE => stop (Wrong (s, arg, "an integer"))
		       | SOME i => i
               in f arg_i; p t end
             | (Real f, arg::t) =>
               let val arg_r =
		       case Real.fromString arg of
			 NONE => stop (Wrong (s, arg, "a real"))
		       | SOME r => r
               in f arg_r; p t end
             | (_, []) => stop (Missing s)
            ) handle Bad m => stop (Message m)
          end
  in
    case CommandLine.arguments () of
      [] => ()
    | l => p l
  end;


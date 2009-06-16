(* Process *)

type status = int

val success : status = 0;

val failure : status = ~1;

prim_val system : string -> status = 1 "sml_system";

fun isSuccess s = s = success

local
    prim_val getenv_ : string -> string = 1 "sys_getenv";
in
    fun getEnv s = (SOME (getenv_ s)) handle _ => NONE
    prim_val sleep  : Time.time -> unit = 1 "sml_sleep";
end

fun isSuccess sv = (sv = success);

val terminate = BasicIO.exit;

local 
    val exittasks = (ref []) : (unit -> unit) list ref
in
    fun atExit newtask =
	exittasks := newtask :: !exittasks;
    fun exit status =
	(List.app (fn f => f ()) (!exittasks);
	terminate status);
end



signature Llvm =
sig
  structure Type :
	    sig
		type t
		(* The type of LLVM types *)

		val bit_size : t -> int
                (*
		 Return the number of bits a given type takes. Only implemented
                 for integer/float types
		 *)

		val assert_pointer_type : t -> unit
		val assert_vector_type  : t -> unit
                (* Predicates for matching certain type structure *)

		val to_output : t -> LlvmOutput.t
                (* Output *)
	    end

  structure Linkage :
	    sig
		type t

		val internal : t
		val to_string : t -> string
	    end

  structure Module :
	    sig
		type t
	    end

  structure BasicBlock :
	    sig
	      type u

	      val simplify : u -> u
	      val to_output : u -> LlvmOutput.t
	    end

  structure Identifier :
	    sig
	      type t
	    end

  structure Value :
	    sig
	      type t
	      type vtable = (Identifier.t, Type.t) LlvmSymtable.t
	      val to_output : t -> LlvmOutput.t
	      (* val type_check : vtable -> t -> (Type.t -> Type.t) *)
	    end

end

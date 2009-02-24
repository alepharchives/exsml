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

signature Llvm =
sig

(* Alignment designations *)
structure Align :
	  sig
	    type t
	    (* The type of Alignments *)
	    val mk : int -> t

	    val output : t -> LlvmOutput.t
	  end

(* Parameter attributes *)
structure ParamAttr :
	  sig
	    datatype t = PA_ZEROEXT | PA_SIGNEXT
		       | PA_INREG | PA_BYVAL | PA_SRET
		       | PA_NOALIAS | PA_NORETURN | PA_NOUNWIND
		       | PA_NEST

	    val assert_funcall_valid : t list -> unit
            (* Assert that our parameter attributes are valid for
	     * function calls
	     *)
	    val output : t -> LlvmOutput.t
	  end

structure CallConv :
	  sig
	    type t

	    val output : t -> LlvmOutput.t
	  end

structure FunctionAttr :
	  sig
	    datatype t = FA_ALWAYSINLINE | FA_NOINLINE | FA_OPTSIZE | FA_NORETURN
		       | FA_NOUNWIND | FA_READNONE | FA_READONLY | FA_SSP | FA_SSPREQ

	    val assert_funcall_valid : t list -> unit
	    (* Assert that these function attributes are valid for function calls *)

	    val output : t -> LlvmOutput.t
	  end

structure Type :
	  sig
	    type t
	    (* The type of LLVM types *)

	    type t_check
	    (* The type of type check assertions *)

	    val bit_size : t -> int
            (* Return the number of bits a given type takes. Not implemented for all types,
	     * but for those where it makes sense *)

	    val 'a run : ('a -> t_check) -> 'a -> unit

	    val assert_pointer : t -> t_check
	    val assert_vector : t -> unit
            (* Predicates for matching certain type structure *)

	    val output : t -> LlvmOutput.t
          (* Output *)
	  end

structure Identifier :
	  sig
	    datatype visibility = Global | Local
	    type t

	    val gensym : unit -> t
	    (* Generate a new identifier with the given visibility. Prefix it with the given
	     * string *)

	    val output : t -> LlvmOutput.t
	  end

structure Linkage :
	  sig
	    type t

	    val internal : t
	    val output : t -> LlvmOutput.t
	  end

structure Module :
	  sig
	    type t
	  end

structure BasicBlock :
	  sig
	    type t

	    val simplify : t -> t
	    val output : t -> LlvmOutput.t
	  end


structure Value :
	  sig
	    type t
	    type vtable = (Identifier.t, Type.t) LlvmSymtable.t
	    val output : t -> LlvmOutput.t
	  (* val type_check : vtable -> t -> (Type.t -> Type.t) *)
	  end
end

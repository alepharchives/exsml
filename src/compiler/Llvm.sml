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

structure Llvm :> Llvm =
struct
  exception Not_Implemented
  (** Global exception. Raised for things that are not implemented yet *)

  exception TypeError of string
  (** Exception raised on type errors *)

  exception Internal_Error of string
  (** Exception raised on internal errors *)

  structure Align =
  struct
    (*
     Alignment is used in LLVM to designate alignments of layouts
     *)
    type t = int

    fun mk align = align

    fun output n = LlvmOutput.integer n
  end

  structure ParamAttr =
  struct
    datatype t = PA_ZEROEXT | PA_SIGNEXT
	       | PA_INREG | PA_BYVAL | PA_SRET
	       | PA_NOALIAS | PA_NORETURN | PA_NOUNWIND
	       | PA_NEST

    fun assert_funcall_valid [] = ()
      | assert_funcall_valid (pattr :: xs) =
	(case pattr of
	  PA_ZEROEXT => assert_funcall_valid xs
	| PA_SIGNEXT => assert_funcall_valid xs
	| PA_INREG   => assert_funcall_valid xs
	| _          => raise TypeError "Invalid Param Attribute in Funcall")

    fun to_string pa =
	case pa of
	  PA_ZEROEXT => "zeroext"
	| PA_SIGNEXT => "signext"
	| PA_INREG   => "inreg"
	| PA_BYVAL   => "byval"
	| PA_SRET    => "sret"
	| PA_NOALIAS => "noalias"
	| PA_NORETURN => "noreturn"
	| PA_NOUNWIND => "nounwind"
	| PA_NEST     => "nest"

    fun output pa = LlvmOutput.str (to_string pa)

  end

  structure FunctionAttr =
  struct
    datatype t = FA_ALWAYSINLINE | FA_NOINLINE | FA_OPTSIZE | FA_NORETURN
	       | FA_NOUNWIND | FA_READNONE | FA_READONLY | FA_SSP | FA_SSPREQ

    fun assert_funcall_valid [] = ()
      | assert_funcall_valid (fnattr :: rest) =
	(case fnattr of
	   FA_NORETURN => assert_funcall_valid rest
	 | FA_NOUNWIND => assert_funcall_valid rest
	 | FA_READONLY => assert_funcall_valid rest
	 | FA_READNONE => assert_funcall_valid rest
	 | _           => raise TypeError "Invalid Function Attribute in funcall")

    fun to_string fa =
	case fa of
	  FA_ALWAYSINLINE => "alwaysinline"
	| FA_NOINLINE => "noinline"
	| FA_OPTSIZE => "optsize"
	| FA_NORETURN => "noreturn"
	| FA_NOUNWIND => "nounwind"
	| FA_READNONE => "readnone"
	| FA_READONLY => "readonly"
	| FA_SSP => "ssp"
	| FA_SSPREQ => "sspreq"

    fun output fa = LlvmOutput.str (to_string fa)
  end

  structure Type =
  struct
    datatype t =
	     (* Integer types *)
	     T_Integer of int
             (* Floating Point types *)
	   | T_Float | T_Double | T_X86fp80 | T_FP128 | T_PPC_FP128
             (* Function types *)
	   | T_Fun of {return: t,
		       params: t list}
	   | T_FunVarArg of {return: t,
			     params: t list}
	     (* Struct types *)
	   | T_Struct of t list
	   | T_PackedStruct of t list
             (* Array/Pointer types *)
	   | T_Array of {ty: t,
			 length: int}
	   | T_Pointer of t
	   | T_QualifiedPointer of {ty: t,
				    address_space: int}
	   | T_Vector of {ty: t,
			  length: int}
	     (* Opaque type, must be unique *)
	   | T_Opaque
             (* Void type, only used for functions returning nothing *)
	   | T_Void
	     (* Label type *)
	   | T_Label
	     (* Top type *)
	   | T_Top

    (* Convenience definitions for easy work on Integer types *)
    val T_I1 = T_Integer 1
    val T_I8 = T_Integer 8
    val T_I16 = T_Integer 16
    val T_I32 = T_Integer 32
    val T_I64 = T_Integer 64
    val T_I128 = T_Integer 128

    fun is_ptr ty =
	case ty of
	  T_Pointer _ => true
	| _ => false

    (* TODO: Test this *)
    fun int_fits_in n k =
	let
	  fun pow2 (0, i) = i
	    | pow2 (n, i) = pow2 (n-1, i*2)
	in
	  n <= pow2 (k, 1)
	end

    fun assert_ptr ty =
	if is_ptr ty then ()
	else raise TypeError "Type is not of pointer type."

    fun assert_sized ty = () (* TODO: Only accept type with specified sizes *)

    fun assert_select_ty ty =
	case ty of
	  T_Integer 1 => true
	| T_Vector {ty = T_I1, ...} => true
	| _ => false

    fun is_struct_type ty =
	case ty of
	  T_Struct _ => true
	| _ => false

    fun is_int ty =
	case ty of
	  T_Integer _ => true
	| _ => false

    fun is_i n ty =
	case ty of
	  T_Integer n => true
	| _ => false

    fun assert_int ty =
	if is_int ty then ()
	else raise TypeError "Type is not of integer type."

    fun is_float ty =
	case ty of
	  T_Float => true
	| T_Double => true
	| T_X86fp80 => true
	| T_FP128 => true
	| T_PPC_FP128 => true
	| _ => false

    fun assert_float ty =
	if is_float ty then ()
	else raise TypeError "Float type expected."

    fun assert_int_float ty =
	if is_int ty orelse is_float ty then ()
	else raise TypeError "Number expected."

    fun is_array_type ty =
	case ty of
	  T_Array _ => true
	| _ => false

    fun is_int_vector ty =
	case ty of
	  T_Vector {length, ty} => is_int ty
	| _ => false

    fun is_float_vector ty =
	case ty of
	  T_Vector {length, ty} => is_float ty
	| _ => false

    fun assert_float_or_vec ty =
	if is_float ty orelse is_float_vector ty then ()
	else raise TypeError "Not Float Scalar nor Vector"

    fun assert_int_or_vec ty =
	if is_int ty orelse is_int_vector ty then ()
	else raise TypeError "Not Int Scalar nor Vector"

    fun is_int_float_vector ty =
	case ty of
	  T_Vector {length, ty} => is_int ty orelse is_float ty
	| _ => false

    fun is_vector ty =
	case ty of
	  T_Vector _ => true
	| _ => false

    fun assert_vector ty =
	if is_vector ty then ()
	else raise TypeError "Type is not of vector type"

    fun assert_float_vector ty =
	if is_float_vector ty then ()
	else raise TypeError "Type is not a float vector type"

    fun assert_int_vector ty =
	if is_int_vector ty then ()
	else raise TypeError "Type is not a float vector type"

    fun is_icmp ty =
	is_int_vector ty orelse is_int ty orelse is_ptr ty

    fun assert_icmp ty =
	if is_icmp ty then ()
	else raise TypeError "Type is not of Icmp Type"

    fun assert_fcmp ty =
	if is_float ty orelse is_float_vector ty then ()
	else raise TypeError "Type is not of fcmp type."

    fun extract_base ty =
	case ty of
	  T_Array {ty, ...} => ty
	| T_Vector {ty, ...} => ty
	| _ => raise Internal_Error "Extracting from a non-extractible"


    (* TODO: Consider fixing assert_eq
     * Rather than having assert_eq assert raw equality, it could
     * try a type unification instead where it will attempt to coerce the
     * two values *)
    fun assert_eq ty1 ty2 =
	if ty1 <> ty2
	then raise TypeError "Types must be equal."
	else ()

    fun extract_size ty =
	case ty of
	  T_Array {length, ...} => length
	| T_Vector {length, ...} => length
	| _ => raise Internal_Error "Extracting from a nen-extractible"

    fun is_first_class ty =
	is_int ty orelse is_float ty orelse
	(case ty of
	   T_Pointer ty' => is_first_class ty'
	 | T_Vector {ty, ...} => is_first_class ty
	 | T_Struct ts => List.all is_first_class ts
	 | T_Array {ty, ...} => is_first_class ty
	 | T_Label => true
	 | _ => false)

    fun assert_both_vec_or_scalar x y =
	let
	  fun is_vector_or_scalar x y =
	      case (x, y) of
		(T_Vector _, T_Vector _) => true
	      | (T_Vector _, _) => false
	      | (_, T_Vector _) => false
	      | _ => true
	in
	  if is_vector_or_scalar x y then ()
	  else raise TypeError "Types are not both vectors or scalars"
	end

    fun assert_first_class ty =
	if is_first_class ty then ()
	else raise TypeError "Type is not of first class type"

    fun assert_first_class_lst args =
	List.app assert_first_class args

    fun bit_size ty =
	let
	  fun sum xs = List.foldr (op+) 0 xs
	in
	  (* TODO: Handle vectors and arrays etc *)
	  case ty of
	    T_Integer i => i
	  | T_Float => 32
	  | T_Double => 64
	  | T_X86fp80 => 80
	  | T_FP128 => 128
	  | T_PPC_FP128 => 128
	  | T_Vector {length, ty} => length * (bit_size ty)
	  | T_Array {length, ty} => length * (bit_size ty)
	  | T_Struct ts => sum (List.map bit_size ts)
	  | _ => raise (Internal_Error "Bit size called with somehting odd")
	end

    fun is_int_range_valid bits integer =
	  bits >= bit_size integer

    fun assert_same_bit_size x y =
	if bit_size x = bit_size y then ()
	else raise TypeError "Bit sizes do not agree"

    fun assert_int_size_gt x y =
	let
	  val xs = bit_size x
	  val ys = bit_size y
	in
	  if xs > ys then ()
	  else raise TypeError "Sizes are wrong!"
	end

    val assert_float_size_gt = assert_int_size_gt

    local
      open LlvmOutput
    in
      fun output ty : LlvmOutput.t =
	  case ty of
	    T_Integer i => (conc [str "i", integer i])
	  | T_Float => (str "float")
	  | T_Double => (str "double")
	  | T_X86fp80 => (str "x86fp80")
	  | T_FP128 => (str "fp128")
	  | T_PPC_FP128 => (str "ppc_fp128")
	  | T_Fun {return, params} =>
	    let
	      val r_t = output return
	      val params_t = conc (List.map output params)
	    in
	      seq_space [r_t, parens params_t]
	    end
	  | T_FunVarArg {return, params} =>
	    let
	      val r_t = output return
	      val params_t = conc (List.map output params)
	    in
	      seq_space [r_t, parens (conc [params_t, str "..."])]
	    end
	   | T_Struct elements =>
	     braces (conc (List.map output elements))
	   | T_PackedStruct elements =>
	     sorround (str "< {") (str "} >")
		      (conc (List.map output elements))
	   | T_Array {ty, length} =>
	     brackets (conc [integer length, str " x ", output ty])
	   | T_Pointer ty => conc [output ty, str " *"]
	   | T_QualifiedPointer {ty, address_space} =>
	       conc [output ty,
		     str " addrspace(", integer address_space, str ")"]
	   | T_Vector {ty, length} =>
	     sorround (str "< ") (str " >")
		      (conc [integer length, str " x ",
			     output ty])
	   | T_Opaque => (str "opaque")
	   | T_Void => (str "void")
	   | T_Label => (str "label")
	   | T_Top => raise (Internal_Error "T_Top is not valid for output")
    end

    fun coercion_error ty_s ty_d =
	TypeError "Coercion error. FIXME: Print out coercion problem"

    (* Type coercion: We are given two types and need to coerce them into
     * being equal types. That is, we need to make sure that the two types
     * are matching, and to generalize the result as much as possible. *)
    (* TODO: Update this function *)
    fun coerce ty_src ty_dst =
	(* Try to coerce something of value ty_src into something of value ty_dst *)
	case (ty_src, ty_dst) of
	  (T_Integer i, T_Integer k) =>
	  if i <= k
	  then T_Integer k
	  else raise (coercion_error (T_Integer i) (T_Integer k))
	| (ty_src, ty_dst) => raise (coercion_error ty_src ty_dst)
  end

  structure CallConv =
  struct
    (** Calling conventions in LLVM *)
    datatype t = CC_C | CC_Fast | CC_Cold | CC_Numbered of int

    fun to_string cc =
	case cc of
	    CC_C => "ccc"
	  | CC_Fast => "fastcc"
	  | CC_Cold => "coldcc"
	  | CC_Numbered i => "cc " ^ Int.toString i

    fun output cc =
	LlvmOutput.str (to_string cc)
  end

  structure Identifier =
  struct
    datatype visibility = Global | Local
    datatype t = Named of visibility * string | Unnamed of int

    val count = ref 0;

    fun gensym () =
	let val v = (!count)
	in
	    count := (!count) + 1;
	    Unnamed v
	end

    fun output id =
	let
	  fun visibility_to_string Local = "%"
	    | visibility_to_string Glbal = "@"
	  fun namer (Named (vis, s)) = (visibility_to_string vis) ^ s
	    | namer (Unnamed i) =
	      let
		val str = "SYM_" ^ Int.toString i
	      in
		namer (Named (Local, str))
	      end
	in
	  LlvmOutput.str (namer id)
	end
  end

  structure Label =
  struct
    type t = string

    fun mk lbl = lbl
    fun to_string lbl = lbl
    fun output x = LlvmOutput.str (to_string x)
  end

  structure Op =
  struct
    (** Integer compares *)
    datatype icmp = IC_EQ | IC_NE | IC_UGT | IC_ULT | IC_ULE | IC_UGE
		  | IC_SGT | IC_SGE | IC_SLT | IC_SLE

    fun icmp_to_string i =
	case i of
	  IC_EQ => "eq"
	| IC_NE => "ne"
	| IC_UGT => "ugt"
	| IC_ULT => "ult"
	| IC_ULE => "ule"
	| IC_UGE => "uge"
	| IC_SGT => "sgt"
	| IC_SGE => "sge"
	| IC_SLT => "slt"
	| IC_SLE => "sle"

    fun output_icmp s = LlvmOutput.str (icmp_to_string s)
    fun output_vicmp s = LlvmOutput.str (icmp_to_string s)

    (** Floating Point compares *)
    datatype fcmp = FC_FALSE | FC_OEQ | FC_OGT | FC_OGE | FC_OLT | FC_OLE
		  | FC_ONE | FC_ORD | FC_UNO | FC_UEQ | FC_UGT | FC_UGE
		  | FC_ULT | FC_ULE | FC_UNE | FC_TRUE

    fun fcmp_to_string x =
	case x of
	  FC_FALSE => "false"
	| FC_OEQ   => "oeq"
	| FC_OGT   => "ogt"
	| FC_OGE   => "oge"
	| FC_OLT   => "olt"
	| FC_OLE   => "ole"
	| FC_ONE   => "one"
	| FC_ORD   => "ord"
	| FC_UNO   => "uno"
	| FC_UEQ   => "ueq"
	| FC_UGT   => "ugt"
	| FC_UGE   => "uge"
	| FC_ULT   => "ult"
	| FC_ULE   => "ule"
	| FC_UNE   => "une"
	| FC_TRUE  => "true"

    fun output_fcmp x = LlvmOutput.str (fcmp_to_string x)
    fun output_vfcmp x = LlvmOutput.str (fcmp_to_string x)

    datatype binop = ADD | SUB | MUL | UDIV | SDIV | FDIV | UREM
		   | SREM | FREM | SHL | LSHR | ASHR | AND | OR | XOR

    datatype conversion = TRUNC | ZEXT | SEXT | FPTRUNC | FPEXT | FPTOUI
			| FPTOSI | UITOFP | SITOFP | PTRTOINT | INTTOPTR
			| BITCAST
    datatype unop = NEG

    fun binop_to_string binop =
	case binop of
	    ADD => "add"
	  | SUB => "sub"
	  | MUL => "mul"
	  | UDIV => "udiv"
	  | SDIV => "sdiv"
	  | FDIV => "fdiv"
	  | UREM => "urem"
	  | SREM => "srem"
	  | FREM => "frem"
	  | SHL  => "shl"
	  | LSHR => "lshr"
	  | ASHR => "ashr"
	  | AND => "and"
	  | OR => "or"
	  | XOR => "xor"

    fun output_binop s = LlvmOutput.str (binop_to_string s)

    fun unop_to_string NEG = "neg"

    fun unop_output s =  LlvmOutput.str (unop_to_string s)

    fun conversion_to_string conv =
	case conv of
	  TRUNC => "trunc"
	| ZEXT  => "zext"
	| SEXT  => "sext"
	| FPTRUNC => "fptrunc"
	| FPEXT   => "fpext"
	| FPTOUI  => "fptoui"
	| FPTOSI  => "fptosi"
	| UITOFP  => "uitofp"
	| SITOFP  => "sitofp"
	| PTRTOINT => "ptrtoint"
	| INTTOPTR => "inttoptr"
	| bitcast  => "bitcast"
    fun conversion_output x = LlvmOutput.str (conversion_to_string x)

    local open LlvmOutput
    in

    fun conversion_output_plug conv src dst =
	seq_space [str (conversion_to_string conv), src, str "to", dst]
    end

  end

  structure Value =
  struct
    datatype compare =
	     C_Icmp of Op.icmp
	   | C_Fcmp of Op.fcmp
	   | C_VIcmp of Op.icmp
	   | C_VFcmp of Op.fcmp

    fun compare_output cmp =
	let
	  fun cmp_output cmp =
	      case cmp of
		C_Icmp icmp => ("icmp", Op.output_icmp icmp)
	      | C_Fcmp fcmp => ("fcmp", Op.output_fcmp fcmp)
	      | C_VIcmp vicmp => ("vicmp", Op.output_vicmp vicmp)
	      | C_VFcmp vfcmp => ("vfcmp", Op.output_vfcmp vfcmp)
	  val (s, cmp_t) = cmp_output cmp
	  open LlvmOutput
	in
	  seq_space [str s, cmp_t]
	end

    type element_ptr_idx = {ty: Type.t,
			    idx: int}

    datatype value = E_Array of typed_value list
		   | E_Binop of {binop: Op.binop,
				 lhs: typed_value,
				 rhs: typed_value}
		   | E_Conversion of {conversion: Op.conversion,
				      value: t,
				      target_ty: Type.t}
		   | E_ExtractElem of {value: typed_value,
				       idx: int}
		   | E_False
	           | E_Float of real
		   | E_GetElementPtr of value * element_ptr_idx list
		   | E_InsertElem  of {value: typed_value,
				       elt: typed_value,
				       idx: int}
		   | E_Int of int
		   | E_Null
		   | E_Select of {cond: typed_value, (* i1 or <N x i1> *)
				  val1: typed_value,
				  val2: typed_value}
		   | E_ShuffleVector of {vec1: value,
					 vec2: value,
					 idxmask: value}
		   | E_String of string
		   | E_Stringz of string
		   | E_Struct of value list
		   | E_True
		   | E_Undef
		   | E_Vector of typed_value list (* Perhaps merge with E_Array *)
		   | E_Zeroinit
    and t = V_Identifier of Identifier.t
	  | V_ConstExpr of value
    withtype typed_value = {value : value, ty : Type.t}

    type vtable = (Identifier.t, Type.t) LlvmSymtable.t

    fun check vtable (check_ty: Type.t) (term: t) : Type.t =
	let
	  fun check_id id = raise Not_Implemented
	  open Type
	  fun check_conversion conversion value target_ty check_ty =
	      target_ty (* TODO: Write this function *)
	  fun check_exp (ty : Type.t) (e : value) : Type.t =
	      case (ty, e) of
		(typ as T_Array {length, ty}, E_Array exp_list) =>
		let
		  fun sametype _ [] = typ
		    | sametype check_ty ({value, ty} :: xs) =
		      if check_ty = ty then
			(check_exp check_ty value;
			 sametype check_ty xs)
		      else
			raise TypeError "Wrong type in array"
		in
		  if List.length exp_list <> length
		  then raise TypeError "Array length/type length mismatch"
		  else sametype ty exp_list
		end
(*	      | (ty, E_Binop {lhs, rhs, ...}) =>
		(check_exp ty lhs;
		 check_exp ty rhs;
		 ty) *)
	      | (ty, E_Conversion {conversion, value, target_ty}) =>
		check_conversion conversion value target_ty ty
	      | (ty, E_ExtractElem {value, idx}) =>
		(* TODO: Punt this for now.
		 * The problem with this call is that we do not
		 * know the type of value. We know that it must have vector
		 * type with ty as the base type contents, but we don't
		 * know the length of the vector. The only way to figure that
		 * out is by doing inference on the constant.
		 *
		 * More reading is required ... *)
		ty
	      | (T_Integer 1, E_False) => T_I1
	      | (T_Double, E_Float real) => T_Double
	        (* TODO: Several other FP Types *)
	      | (ty, E_GetElementPtr (exp, idx_list)) =>
		(* TODO: Punt this for now. Complex instruction *)
		ty
	      | (ty, E_InsertElem {value, elt, idx}) =>
		(* TODO: Punt for now. Must check several things *)
		ty
	      | (T_Integer k, E_Int n) =>
		if int_fits_in n k then T_Integer k
		else raise TypeError "Integer does not fit into type"
	      | (T_Pointer ty, E_Null) => T_Pointer ty
	      | (ty, E_Select {cond, val1, val2}) =>
		let
		  val {value = val1_value, ty = val1_ty} = val1
		  val {value = val2_value, ty = val2_ty} = val2
		  val {value = cond_value, ty = cond_ty} = cond
		in
		  (* TODO: Assertions! *)
		  if val1_ty = val2_ty
		  then if val1_ty = ty
		       then
			 (check_exp ty val1_value;
			  check_exp ty val2_value;
			  check_exp Type.T_I1 cond_value;
			  ty)
		       else
			 raise TypeError "Binop operand/return type mismatch"
		  else
		    raise TypeError "Binop types mismatch"
		end
	      | (ty, E_ShuffleVector {vec1, vec2, idxmask}) =>
		let
		  val ty' = Type.T_Vector {length = Type.extract_size ty,
					   ty = Type.T_I1}
		in
		  check_exp ty vec1;
		  check_exp ty vec2;
		  check_exp ty' idxmask
		end
	      (* TODO: E_Shufllevector *)
	      | (ty as T_Array {length, ty = T_Integer 8}, E_String str) =>
		if String.size str <> length
		then raise TypeError "String has wrong size"
		else ty
	      | (ty as T_Array {length, ty = T_Integer 8}, E_Stringz str) =>
		if String.size str + 1 <> length
		then raise TypeError "Stringz has wrong size"
		else ty
	      | (T_Struct ts, E_Struct es) =>
		let
		  fun check_struct [] [] = T_Struct ts
		    | check_struct (t::ts) (e::es) =
		      (check_exp t e;
		       check_struct ts es)
		    | check_struct _ _ =
		      raise TypeError "Type/Exp len mismatch in struct"
		in
		  check_struct ts es
		end
	      | (ty as T_Integer 1, T_True) => ty
	      | (ty, E_Undef) => ty
	      | (typ as T_Vector {length, ty = check_ty}, E_Vector elist) =>
		let
		  fun sametype [] = ()
		    | sametype ({value, ty} :: es) =
		      if ty = check_ty then
			(check_exp check_ty value;
			 sametype es)
		      else
			raise TypeError "Vector type is wrong"
		in
		  if List.length elist <> length
		  then raise TypeError "Vector arity mismatch"
		  else (sametype elist;
			typ)
		end
	      | (ty, E_Zeroinit) => ty
	      | (ty, exp) =>
		raise TypeError "Type/Exp mismatch (Alter me to print out the types!)"
	in
	  case term of
	    V_Identifier id => check_id id
	  | V_ConstExpr value => check_exp check_ty value
	end

    fun output (term : t) : LlvmOutput.t =
	let
	  open LlvmOutput
	  fun output_typed_exp {ty, value} =
	      seq_space [Type.output ty, output_exp value]
	  and output_exp (exp : value) =
	      case exp of
		E_Array el => brackets (commas (List.map output_typed_exp el))
	      | E_Binop {binop, lhs, rhs} =>
		seq_space [Op.output_binop binop,
			   parens (commas [output_typed_exp lhs,
					   output_typed_exp rhs])]
	      | E_Conversion {conversion, value, target_ty} =>
			    seq_space [Op.conversion_output conversion,
				       parens (seq_space [output_exp exp, str "to", Type.output target_ty])]
	      | E_ExtractElem {value, idx} =>
		seq [str "extractelement", parens (commas [output_typed_exp value, integer idx])]
	      | E_False => str "false"
	      | E_Float r => real r
	      | E_GetElementPtr (exp, indexes) =>
		let
		  fun output_idxs {ty, idx} = integer idx
		in
		  seq [str "getelementptr", parens (commas
						      [output_exp exp,
						       commas (List.map output_idxs indexes)])]
		end
	      | E_InsertElem {value, elt, idx} =>
		seq [str "insertelement", parens (commas [output_typed_exp value,
							  output_typed_exp elt,
							  integer idx])]
	      | E_Int i => integer i
	      | E_Null => str "null"
	      | E_Select {cond, val1, val2} =>
		seq [str "select", parens (commas (List.map output_typed_exp
							    [cond, val1, val2]))]
	      | E_ShuffleVector {vec1, vec2, idxmask} =>
		seq [str "shufflevector", parens (commas [output_exp vec1,
							  output_exp vec2,
							  output_exp idxmask])]
	      | E_String s => seq_space [str "\"", str s, str "\""]
	      | E_Stringz s => str (s ^ "\000") (* Ugly, hopefully it is right *)
	      | E_Struct ts => braces (commas (List.map output_exp ts))
	      | E_True => str "true"
	      | E_Undef => str "undef"
	      | E_Vector el => vector (commas (List.map output_typed_exp el))
	      | E_Zeroinit => str "zeroinitializer"
	in
	  case term of
	    V_ConstExpr v => output_exp v
	  | V_Identifier id => Identifier.output id
	end

    fun is_constant (V_ConstExpr _) = true
      | is_constant _             = false

    fun const_float r = E_Float r
    fun const_string str = E_String str
    fun const_stringz str = E_Stringz str
  end

  structure BasicBlock =
  struct
    datatype u =
               (* Binary operations *)
	         S_BinOp of {binop: Op.binop,
			     ty: Type.t,
			     lhs: Value.t,
			     rhs: Value.t,
			     ret: Identifier.t,
			     name: string option}
	       (* Unary operations *)
	       | S_Unop of {unop: Op.unop,
			    ty: Type.t,
			    ret: Identifier.t,
			    value: Value.t,
			    name: string option}
	       (* Branch operations *)
	       | S_Ret of (Type.t * Value.t) option
	       | S_Branch of {cond: Value.t, (* Has boolean type *)
			      label_t: Label.t,
			      label_f: Label.t}
	       | S_UBranch of Label.t
	       | S_Switch of {ty: Type.t,
			      value: Value.t,
			      default: Label.t,
			      cases: switch_case list}
	       | S_Invoke of {callconv: CallConv.t,
			      ret_attrs: ParamAttr.t list,
			      func: Value.t,
			      func_ty: Type.t,
			      args: Value.t list,
			      func_attrs: FunctionAttr.t list,
			      label_cont: Label.t,
			      label_unwind: Label.t,
			      result: Identifier.t}
	       | S_Unwind
	       | S_Unreachable
               (* Vector operations *)
	       | S_ExtractElement of {result: Identifier.t,
				      ty: Type.t, (* Ass: Vector type *)
				      value: Value.t,
				      idx: Value.t} (* Assumption: i32 *)
	       | S_InsertElement of {ty: Type.t,  (* Vector type *)
				     value: Value.t,
				     elem_ty: Type.t, (* Match: type in vector*)
				     elem_value: Value.t,
				     idx: Value.t} (* i32 ty *)
	       | S_ShuffleVector of {result: Identifier.t,
				     v1_ty: Type.t,
				     v1: Value.t,
				     v2_ty: Type.t,
				     v2: Value.t,
				     mask_len: int,
				     mask: Value.t}
	       (* Aggregate operations *)
               | S_ExtractValue of {result: Identifier.t,
				    ty: Type.t,
				    value: Value.t,
				    idxs: Value.t list} (* Not empty *)
	       | S_InsertValue of {ty: Type.t,
				   value: Value.t,
				   elem_ty: Type.t,
				   elem_value: Value.t,
				   idx: Value.t}
               (* Memory Access *)
	       | S_Malloc of {result: Identifier.t,
			      ty: Type.t,
			      num_elems: int,
			      align: Align.t option}
	       | S_Free of {ty: Type.t,
			    value: Value.t}
	       | S_Alloca of {result: Identifier.t,
			      ty: Type.t,
			      num_elems: int,
			      align: Align.t option}
	       | S_Load of {result: Identifier.t,
			    volatile: bool,
			    ty: Type.t,
			    value: Value.t,
			    align: Align.t option}
	       | S_Store of {volatile: bool,
			     ty: Type.t,
			     value: Value.t,
			     ptr_ty: Type.t,
			     ptr: Value.t,
			     align: Align.t option}
	       | S_GetElementPtr of {result: Identifier.t,
				     ty: Type.t,
				     value: Value.t,
				     idxs: (Type.t * int) list}
	       (* Conversion Operations *)
	       | S_Conversion of {result: Identifier.t,
				  conversion: Op.conversion,
				  src_ty: Type.t,
				  value: Value.t,
				  dst_ty: Type.t}
               (* Other operations *)
	       | S_Icmp of {result: Identifier.t,
			    cond: Op.icmp,
			    ty: Type.t,
			    lhs: Value.t,
			    rhs: Value.t}
	       | S_Fcmp of {result: Identifier.t,
			    cond: Op.fcmp,
			    ty: Type.t,
			    lhs: Value.t,
			    rhs: Value.t}
	       | S_VIcmp of {result: Identifier.t,
			     cond: Op.icmp,
			     ty: Type.t,
			     lhs: Value.t,
			     rhs: Value.t}
	       | S_VFcmp of {result: Identifier.t,
			     cond: Op.fcmp,
			     ty: Type.t,
			     lhs: Value.t,
			     rhs: Value.t}
	       | S_Phi of {result: Identifier.t,
			   ty: Type.t,
			   predecs: (Value.t * Label.t) list}
	       | S_Select of {result: Identifier.t,
			      ty: Type.t,
			      cond: Value.t,
			      true_ty: Type.t,
			      true_value: Value.t,
			      false_ty: Type.t,
			      false_value: Value.t}
	       | S_Call of {func: Value.t,
			    tail: bool, (* Tail call optim. applicable *)
			    call_conv: CallConv.t option,
			    param_attrs: ParamAttr.t list,
			    fn_attrs: FunctionAttr.t list,
			    ty: Type.t,
			    fnty: Type.t option,
			    args: Value.t list,
			    ret: Identifier.t,
			    name: string} (* Maybe Identifier *)
               (* Basic Block sequencing *)
	       | S_Seq of u list
	       | S_Conc of u * u
    withtype switch_case = {ty: Type.t,
			    value: Value.t,
			    label: Label.t}
    type t = Label.t * u

    fun check vtable bb =
	case bb of
	  S_Alloca {result, ty, num_elems, align} =>
	  (Type.assert_sized ty;
	   LlvmSymtable.enter result (Type.T_Pointer ty) vtable)
	| S_BinOp {binop, ty, lhs, rhs, ret, ...} =>
	  let
	    fun bin_coerce ty lhs rhs =
		let
		  val lhs_ty = Value.check vtable ty lhs
		in
		  Value.check vtable ty rhs
		end
	  in case binop of
	       Op.ADD => (Type.assert_int_float ty;
			  LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.SUB => (Type.assert_int_float ty;
			  LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.MUL => (Type.assert_int_float ty;
			  LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.UDIV => (Type.assert_int ty;
			   LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.SDIV => (Type.assert_int ty;
			   LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.FDIV => (Type.assert_float ty;
			   LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.UREM => (Type.assert_int ty;
			   LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.SREM => (Type.assert_int ty;
			   LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.FREM => (Type.assert_float ty;
			   LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)

	     | Op.SHL => (Type.assert_int ty;
			  LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.LSHR => (Type.assert_int ty;
			   LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.ASHR => (Type.assert_int ty;
			   LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.AND => (Type.assert_int ty;
			  LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.OR => (Type.assert_int ty;
			  LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	     | Op.XOR => (Type.assert_int ty;
			  LlvmSymtable.enter ret (bin_coerce ty lhs rhs) vtable)
	  end
	| S_Branch {cond, label_t, label_f} =>
	  (Value.check vtable Type.T_I1 cond;
	   vtable)
	| S_Call {func, tail, ty, call_conv, args, ret, name, param_attrs, fnty, fn_attrs} =>
	  (* Rules:
	     DONE:
	     1. Of the param attributes, only 'zeroext, signext and inreg' are valid.
             8. Only function attributes of the kinds 'noreturn', 'nounwind',
                'readonly' and 'readnone' are valid attribute kinds here
	     2. 'ty' is the return type of the function.
             7. Function attributes are optional.

             NEEDS ATTENTION:
             3. fnty is the function type but it is only needed when playing with function
	        pointer returns and varargs. Sigh.
                (What is the problem here?? We need to se an example or two to figure this
                 guy out. It is probably somehting simple)

             SOLVED:
             4. func is the 'fnptrval' containing the function being invoked.
                (this is usually an identifier (always?? doesn't matter. It will work)

             5. args should match the function signature args.
                (Can be tested by coercion of the type signature and a constructed
                 signature for the funcall)

             6. All args are firstclass.
                (Can be checked by walking the types in the args, which has to be there
                 or else we can't figure out the types of the crap. Coerce the values to the
                 types)
	   *)
	  (* TODO: The S_Call is wrong. Fix it! *)
	  (ParamAttr.assert_funcall_valid param_attrs;
	   FunctionAttr.assert_funcall_valid fn_attrs;
	   (* Type.assert_first_class_lst args; *) (* TODO: Figure out what we really need here *)
	   LlvmSymtable.enter ret ty vtable)
	| S_Conc (u,v) => check vtable (S_Seq [u, v])
	| S_ExtractElement {result, ty, value, idx} =>
	  (* Rules:
             1. idx must be a I32
             2. ty must be a vector type.
             3. value must coerce to the vector type *)
	  (Type.assert_vector ty;
	   Value.check vtable Type.T_I32 value;
	   let
	     val r_ty = Value.check vtable ty value
	   in
	     LlvmSymtable.enter result r_ty vtable
	   end)
	| S_Free {ty, value} =>
	  (Type.assert_ptr ty;
	   Value.check vtable ty value;
	  vtable)
	| S_InsertElement {ty, value, elem_ty, elem_value, idx} =>
	  (* Rules:
	     1. Ty must be a vector type.
             2. Value must coerce to ty
	     3. elem_ty must be the extract of the vector type
             4. elem_value must coerce to elem_ty
	     5. idx must coerce to T_I32.
	     Function is void'ed
          *)
	  (Type.assert_vector ty;
	   Value.check vtable ty value;
	   Type.assert_eq (Type.extract_base ty) elem_ty;
	   Value.check vtable elem_ty elem_value;
	   Value.check vtable Type.T_I32 idx;
	   vtable)
	| S_Invoke {callconv, ret_attrs, func, func_ty, args, func_attrs, label_cont,
		    label_unwind, result} =>
	  (* Rules:
             1. ty_attrs are parameter attributes. Only 'zeroext','signext' and 'inreg' are valid.
             2. func_ty is the signature of the function being invoked.
	     3. func is the value of the function in question.
             4. args are the function arguments. They must match the function signature type.
             5. Of the function attributes list, only 'noreturn', 'nounwind', 'readonly'
                and 'readnone' are valid.
           *)
	  (* TODO! *)
	  LlvmSymtable.enter result func_ty vtable (* Wrong, should extract ret-ty from func_ty *)
	| S_ShuffleVector {result, v1_ty, v1, v2_ty, v2, mask_len, mask} =>
	  (* Rules:
	     1. v1_ty and v2_ty must agree
	     2. v1 and v2 must coerce to their respective types
             3. mask must coerce to mask_ty
             4. mask_ty must be a valid mask type
	     must update result to have the type of v1_ty *)
	  let
	    val v1_ty' = Value.check vtable v1_ty v1
	    val v2_ty' = Value.check vtable v2_ty v2
	    val mask_ty = Type.T_Vector {length = mask_len, ty = Type.T_I32}
	    val mask_ty' = Value.check vtable mask_ty mask
	  in
	    Type.assert_eq v1_ty' v2_ty';
	    LlvmSymtable.enter result v1_ty' vtable
	  end
	| S_ExtractValue {result, ty, value, idxs} =>
	  (* Rules:
	     1. value and ty must coerce.
             2. idxs must be a list of T_I32's *)
	  let
	    val ty' = Value.check vtable ty value
	  in
	    (* TODO: Reinstate this! *)
	    (* Value.assert_idx_list vtable idxs; *)
	    LlvmSymtable.enter result ty' vtable
	  end
	| S_InsertValue {ty, value, elem_ty, elem_value, idx} =>
	  (* Rules:
	     1. Value and ty must coerce
             2. Elem and elem_ty must coerce
             3. idx is a T_I32.
             4. extract-type of ty and elem_ty agrees
             return is void *)
	  let
	    val ty' = Value.check vtable ty value
	    val elem_ty' = Value.check vtable elem_ty elem_value
	  in
	    Value.check vtable Type.T_I32 idx;
	    Type.assert_eq (Type.extract_base ty') elem_ty';
	    vtable
	  end
	| S_Phi {result, ty, predecs} =>
	  (* Check the all predecessors are valid *)
	  let
	    fun check_predecs [] = ()
	      | check_predecs ((v, l)::rest) =
		 (Value.check vtable ty v;
		  check_predecs rest)
		(* TODO: Also check label uniqueness *)
	  in
	    Type.assert_first_class ty;
	    check_predecs predecs;
	    LlvmSymtable.enter result ty vtable
	  end
	| S_Malloc {result, ty, num_elems, align} =>
	  (Type.assert_sized ty;
	   LlvmSymtable.enter result (Type.T_Pointer ty) vtable)
	| S_Load {result, volatile, ty, value, align} =>
	  (Type.assert_ptr ty;
	   Type.assert_first_class ty;
	   let
	     val v_ty = Value.check vtable ty value
	   in
	     LlvmSymtable.enter result v_ty vtable
	   end)
	| S_Ret NONE => vtable
	| S_Ret (SOME (ty, value)) =>
	  (Value.check vtable ty value;
	   vtable)
	| S_Select {result, ty, cond,
		    true_ty, true_value,
		    false_ty, false_value} =>
	  (* Rules:
	     1. true_ty and false_ty must agree
             2. Values must coerce to their types
	     3. The cond must coerce to the select type
 	     4. The select ty must be the valid types, either I1 or a Vector
                of I1's
	   *)
	  if true_ty <> false_ty
	  then raise TypeError "True and False type branches disagree."
	  else (Value.check vtable true_ty true_value;
		Value.check vtable false_ty false_value;
		Value.check vtable ty cond;
		Type.assert_select_ty ty;
		LlvmSymtable.enter result true_ty vtable)
	| S_GetElementPtr {result, ty, value, idxs} =>
	  (* This is a bitch to implement *)
	  raise Not_Implemented
	| S_Icmp {result, cond, ty, lhs, rhs} =>
	  (* Comparator of integers. Rules:
	     1. Ty must be one of 'integer', 'pointer', or 'vector integer' type.
	     2. Both lhs and rhs must coerce to ty.
	     3. Types of lhs and rhs must agree.
             4. The result is always either i1 or vector i1. *)
	  let
	    val ty' = Value.check vtable ty lhs
	    val ty'' = Value.check vtable ty' rhs
	  in
	    Type.assert_icmp ty'';
	    LlvmSymtable.enter
	      result
	      (if Type.is_vector ty''
	       then
		  let val v_len = Type.extract_size ty''
		  in Type.T_Vector {length = v_len, ty = Type.T_I1}
		  end
	       else Type.T_I1)
	      vtable
	  end
	| S_Conversion {result, conversion, src_ty, value, dst_ty} =>
	  (* Rules dependent on conversion type *)
	  (Value.check vtable src_ty value;
	   (case conversion of
	      Op.TRUNC =>
	        (Type.assert_int src_ty;
		 Type.assert_int dst_ty;
		 Type.assert_int_size_gt src_ty dst_ty)
	    | Op.ZEXT =>
	        (Type.assert_int src_ty;
		 Type.assert_int dst_ty;
		 Type.assert_int_size_gt dst_ty src_ty)
	    | Op.SEXT =>
	        (Type.assert_int src_ty;
		 Type.assert_int dst_ty;
		 Type.assert_int_size_gt dst_ty src_ty)
	    | Op.FPTRUNC =>
	        (Type.assert_float src_ty;
		 Type.assert_float dst_ty;
		 Type.assert_float_size_gt src_ty dst_ty)
	    | Op.FPEXT =>
	        (Type.assert_float src_ty;
		 Type.assert_float dst_ty;
		 Type.assert_float_size_gt dst_ty src_ty)
	    | Op.FPTOUI =>
	        (Type.assert_float_or_vec src_ty;
		 Type.assert_int_or_vec dst_ty;
		 Type.assert_both_vec_or_scalar src_ty dst_ty)
	    | Op.FPTOSI =>
	        (Type.assert_float_or_vec src_ty;
		 Type.assert_int_or_vec dst_ty;
		 Type.assert_both_vec_or_scalar src_ty dst_ty)
	    | Op.UITOFP =>
	        (Type.assert_int_or_vec src_ty;
		 Type.assert_float_or_vec dst_ty;
		 Type.assert_both_vec_or_scalar src_ty dst_ty)
	    | Op.SITOFP =>
	        (Type.assert_int_or_vec src_ty;
		 Type.assert_float_or_vec dst_ty;
		 Type.assert_both_vec_or_scalar src_ty dst_ty)
	    | Op.PTRTOINT =>
	        (Type.assert_ptr src_ty;
		 Type.assert_int dst_ty)
	    | Op.INTTOPTR =>
	        (Type.assert_int src_ty;
		 Type.assert_ptr dst_ty)
	    | Op.BITCAST =>
	        (Type.assert_first_class src_ty;
		 Type.assert_first_class dst_ty;
		 Type.assert_same_bit_size src_ty dst_ty));
	     LlvmSymtable.enter
	       result
	       dst_ty
	       vtable)
	| S_Fcmp {result, cond, ty, lhs, rhs} =>
	  (* Comparator for integers. Rules:
	     1. ty must be one of float or vector float.
             2. Both lhs and rhs must coerce.
	     3. Types of phs and rhs must agree
             4. The result is i1 or vector i1 *)
	  let
	    val ty' = Value.check vtable ty lhs
	    val ty'' = Value.check vtable ty' rhs
	  in
	    Type.assert_fcmp ty'';
	    LlvmSymtable.enter
	      result
	      (if Type.is_vector ty''
	       then
		 let val v_len = Type.extract_size ty''
		 in Type.T_Vector {length = v_len, ty = Type.T_I1}
		 end
	       else Type.T_I1)
	    vtable
	  end
	| S_VIcmp {result, cond, ty, lhs, rhs} =>
	  (* Rules:
	     1. ty is always an intger vector.
             2. lhs and rhs must coerce.
             3. result type is ty.
           *)
          let
	    val ty' = Value.check vtable ty lhs
	    val ty'' = Value.check vtable ty' rhs
	  in
	    Type.assert_int_vector ty;
	    LlvmSymtable.enter result ty'' vtable
	  end
	| S_VFcmp {result, cond, ty, lhs, rhs} =>
	  (* Rules similar to VIcmp *)
	  let
	    val ty' = Value.check vtable ty lhs
	    val ty'' = Value.check vtable ty' rhs
	  in
	    Type.assert_float_vector ty;
	    LlvmSymtable.enter result ty'' vtable
	  end
	| S_Seq instructions =>
	  let
	    fun process_instructions [] vtable = vtable
	      | process_instructions (i::is) vtable =
		process_instructions is (check vtable i)
	  in
	    process_instructions instructions vtable
	  end
	| S_Store {volatile, ty, value, ptr_ty, ptr, align} =>
	  (Type.assert_ptr ptr_ty;
	   Value.check vtable ty value;
	   Value.check vtable ptr_ty ptr;
	   vtable)
	| S_Switch {ty, value, default, cases} =>
	  let
	    fun assert_cases_unique seen [] = ()
	      | assert_cases_unique seen ({ty, value, label} :: rest) =
		(Type.assert_int ty;
		 Value.check vtable ty value;
		 if List.exists (fn x => x = value) seen
		 then raise TypeError "Switch cases not unique"
		 else assert_cases_unique (value::seen) rest)
	  in
	    Type.assert_int ty;
	    Value.check vtable ty value;
	    assert_cases_unique [] cases;
	   vtable
	  end
	| S_UBranch lbl => vtable
	| S_Unwind => vtable
	| S_Unreachable => vtable
	| _ => raise Not_Implemented

    local
	fun con_binop binop ty ret op1 op2 = S_BinOp {binop = binop,
						      ty = ty,
						      lhs = op1,
						      rhs = op2,
						      ret = ret,
						      name = NONE}
	fun con_binop_n binop str ty ret op1 op2 =
	    S_BinOp {binop = binop,
		     ty = ty,
		     lhs = op1,
		     rhs = op2,
		     ret = ret,
		     name = SOME str}
    in

    fun ret ty v = S_Ret (SOME (ty, v))
    val ret_void = S_Ret NONE

    val add = con_binop Op.ADD
    val add_n = con_binop_n Op.ADD

    val sub = con_binop Op.SUB
    val sub_n = con_binop_n Op.SUB

    val mul = con_binop Op.MUL
    val mul_n = con_binop_n Op.MUL

    val udiv = con_binop Op.UDIV
    val udiv_n = con_binop_n Op.UDIV

    val sdiv = con_binop Op.SDIV
    val sdiv_n = con_binop_n Op.SDIV

    val fdiv = con_binop Op.FDIV
    val fdiv_n = con_binop_n Op.SDIV

    val urem = con_binop Op.UREM
    val urem_n = con_binop_n Op.UREM

    val srem = con_binop Op.SREM
    val srem_n = con_binop_n Op.SREM

    val fdiv = con_binop Op.FDIV
    val fdiv_n = con_binop_n Op.SDIV

    val urem = con_binop Op.UREM
    val urem_n = con_binop_n Op.UREM

    val srem = con_binop Op.SREM
    val srem_n = con_binop_n Op.SREM

    val frem = con_binop Op.FREM
    val frem_n = con_binop_n Op.FREM

    val shl = con_binop Op.SHL
    val shl_n = con_binop_n Op.SHL

    val lshr = con_binop Op.LSHR
    val lshr_n = con_binop_n Op.LSHR

    val ashr = con_binop Op.ASHR
    val ashr_n = con_binop_n Op.ASHR

    val and_ = con_binop Op.AND
    val and_n_ = con_binop_n Op.AND

    val or = con_binop Op.OR
    val or_n = con_binop_n Op.OR

    val xor = con_binop Op.XOR
    val xor_n = con_binop_n Op.XOR

    end

    local
      open LlvmOutput
    in

    fun output (label, operation) =
	let
	  fun output_op operation =
	      case operation of
		S_Ret NONE => str "ret void"
	      | S_Ret (SOME (ty, v)) =>
		seq_space [str "ret", Type.output ty, Value.output v]
	      | S_BinOp {binop, ty, lhs, rhs, ret, ...} =>
		seq_space [Identifier.output ret, str "=",
			   Op.output_binop binop,
			   Type.output ty,
			   Value.output lhs, str ",",
			   Value.output rhs]
	      | S_Unop {unop, ty, value, ret, ...} =>
		  seq_space [
		    Identifier.output ret, str "=",
		    Op.unop_output unop,
		    Type.output ty,
		    Value.output value]
	      | S_Branch {cond, label_t, label_f} =>
		  seq_space [
		    str "br", str "i1", Value.output cond,
		    str ", label", Label.output label_t,
		    str ", label", Label.output label_f]
	      | S_UBranch lbl =>
		  seq_space [str "br", Label.output label]
	      | S_Switch {ty, value, default, cases} =>
		let
		  fun output_case {ty, value, label} =
		      seq_space [
		        Type.output ty, Value.output value, str ",",
			str "label", Label.output label]
		  fun output_cases cases =
		      seq_space (List.map output_case cases)
		in
		  seq_space [str "switch", Type.output ty, Value.output value,
			     str ", label", Label.output default, output_cases cases]
		end
	      | S_Invoke {callconv, ret_attrs, func, func_ty, args, func_attrs,
			  label_cont, label_unwind, result} =>
		let
		  fun process_args args = commas (List.map Value.output args)
		in
		  seq_space [Identifier.output result, str "= invoke",
			     CallConv.output callconv,
			     case ret_attrs of
			       [] => null
			     | attrs => seq_space (List.map ParamAttr.output attrs),
			     Type.output func_ty,
			     seq [Value.output func, parens (process_args args)],
			     case func_attrs of
			       [] => null
			     | attrs => seq_space (List.map FunctionAttr.output attrs),
			     str "to label", Label.output label_cont,
			     str "unwind label", Label.output label_unwind]
		end
	      | S_Unwind => str "unwind"
	      | S_Unreachable => str "unreachable"
	      | S_ExtractElement {result, ty, value, idx} =>
		seq_space [Identifier.output result, str "= extractelement",
			   Type.output ty, Value.output value, str ", i32",
			   Value.output idx]
	      | S_InsertElement {ty, value, elem_ty, elem_value, idx} =>
		seq_space [str "insertelement",
			   Type.output ty, Value.output value, str ",",
			   Type.output elem_ty, Value.output elem_value,
			   str ", i32", Value.output idx]
	      | S_ShuffleVector {result, v1_ty, v1, v2_ty, v2, mask_len, mask} =>
		seq_space [Identifier.output result, str "= shufflevector",
			   Type.output v1_ty, Value.output v1,
			   Type.output v2_ty, Value.output v2,
			   Type.output (Type.T_Vector {ty = Type.T_I32, length = mask_len}),
			   Value.output mask]
	      | S_ExtractValue {result, ty, value, idxs} =>
		let
		  fun output_idx idx = seq_space [str "i32",
						  Value.output idx]
		  val output_idxs = intersperse (str ",") (List.map output_idx idxs)
		in
		  seq_space [Identifier.output result, str "= extractvalue",
			     Type.output ty, Value.output value, str ", ",
			     output_idxs]
		end
	      | S_InsertValue {ty, value, elem_ty, elem_value, idx} =>
		seq_space [str "insertvalue", Type.output ty, Value.output value,
			   str ",", Type.output elem_ty, Value.output elem_value,
			   str ", i32", Value.output idx]
	      | S_Malloc {result, ty, num_elems, align} =>
		seq_space [Identifier.output result, str "=", Type.output ty,
			   str ", i32", integer num_elems, case align of
							     NONE => null
							   | SOME a => seq_space [str ", align",
										  Align.output a]]
	      | S_Free {ty, value} =>
		seq_space [str "free", Type.output ty, Value.output value]
	      | S_Alloca {result, ty, num_elems, align} =>
		commas [seq_space [Identifier.output result, str "=", Type.output ty],
			seq_space [str "i32", integer num_elems],
			case align of NONE => null | SOME a =>
						       seq_space [str "align", Align.output a]]

	      | S_Load {result, volatile, ty, value, align} =>
		commas [seq_space [Identifier.output result, str "=",
				   if volatile then str "volatile" else null,
				   str "load", Type.output ty, Value.output value],
			case align of
			  NONE => null
			| SOME a => seq_space [str "align", Align.output a]]
	      | S_Store {volatile, ty, value, ptr_ty, ptr, align} =>
		commas [seq_space [if volatile then str "volatile" else null,
				   str "store", Type.output ty, Value.output value],
			seq_space [Type.output ptr_ty, Value.output ptr],
			case align of
			  NONE => null
			| SOME a => seq_space [str "align", Align.output a]]
	      | S_GetElementPtr {result, ty, value, idxs} =>
		let
		  fun process_idxs idxs = commas (List.map (fn (ty, i) =>
							       seq_space [Type.output ty,
									  integer i])
							   idxs)
		in
		  seq_space [Identifier.output result, str "=",
			     Type.output ty, Value.output value,
			     process_idxs idxs]
		end
	      | S_Conversion {result, conversion, src_ty, value, dst_ty} =>
		let
		  val src_line = seq_space [Type.output src_ty, Value.output value]
		  val dst_line = Type.output dst_ty
		in
		  seq_space [Identifier.output result, str "=",
			     Op.conversion_output_plug conversion src_line dst_line]
		end
	      | S_Icmp {result, cond, ty, lhs, rhs} =>
		seq_space [Identifier.output result, str "= icmp", Op.output_icmp cond,
			   Type.output ty,
			   commas [Value.output lhs, Value.output rhs]]
	      | S_Fcmp {result, cond, ty, lhs, rhs} =>
		seq_space [Identifier.output result, str "= fcmp", Op.output_fcmp cond,
			   Type.output ty,
			   commas [Value.output lhs, Value.output rhs]]
	      | S_VIcmp {result, cond, ty, lhs, rhs} =>
		seq_space [Identifier.output result, str "= vicmp", Op.output_vicmp cond,
			   Type.output ty,
			   commas [Value.output lhs, Value.output rhs]]
	      | S_VFcmp {result, cond, ty, lhs, rhs} =>
		seq_space [Identifier.output result, str "= vfcmp", Op.output_vfcmp cond,
			   Type.output ty,
			   commas [Value.output lhs, Value.output rhs]]
	      | S_Phi {result, ty, predecs} =>
		let
		  fun output_predecs predecs =
		      commas (List.map (fn (value, label) =>
					   commas [Value.output value, Label.output label])
				       predecs)
		in
		  seq_space [Identifier.output result, str "= phi",
			     Type.output ty,
			     output_predecs predecs]
		end
	      | S_Select {result, ty, cond,
			  true_ty, true_value,
			  false_ty, false_value} =>
		commas [seq_space [Identifier.output result, str "= select",
				   Type.output ty, Value.output cond],
			seq_space [Type.output true_ty, Value.output true_value],
			seq_space [Type.output false_ty, Value.output false_value]]
	      | S_Call {func, tail, call_conv, param_attrs, ty, fnty, args, ret, name, fn_attrs} =>
		let
		  fun process_args args = commas (List.map Value.output args)
		in
		  seq_space [Identifier.output ret, str "=",
			     if tail then str "tail" else null,
			     str "call",
			     case call_conv of
			       NONE => null
			     | SOME cconv => CallConv.output cconv,
			     case param_attrs of
			       [] => null
			     | p_attrs => seq_space (List.map ParamAttr.output p_attrs),
			     Type.output ty,
			     case fnty of
			       NONE => null
			     | SOME t => Type.output t,
			     seq [Value.output func, parens (process_args args)],
			     case fn_attrs of
			       [] => null
			     | attr => seq_space (List.map FunctionAttr.output attr)]
		end
	      | S_Seq us =>
		intersperse (str "\n") (List.map output_op us)
	      | S_Conc (u, t) => output_op (S_Seq [u, t])
	in
	  LlvmOutput.conc
	    [Label.output label, LlvmOutput.str ":\n",
	     output_op operation]
	end
    end

    (* Simplify term-rewrites a BB until it is canonical to the LLVM system *)
    fun simplify bb = bb

  end

  structure Linkage =
  struct
    (** Linkage describes the linkage used for an IR Module *)
    datatype t = Link_Private
	       | Link_Internal
	       | Link_Linkonce
	       | Link_Common
	       | Link_Weak
	       | Link_Appending
	       | Link_Extern_Weak
	       | Link_Extern_Visible
	       (* Microsoft link types *)
	       | Link_Dllimport
	       | Link_Dllexport

    (* Convenience shortcuts *)
    val internal = Link_Internal

    fun to_string linkage =
	case linkage of
	  Link_Private        => "private"
	| Link_Internal       => "internal"
	| Link_Linkonce       => "linkonce"
	| Link_Common         => "common"
	| Link_Weak           => "weak"
	| Link_Appending      => "appending"
	| Link_Extern_Weak    => "extern_weak"
	| Link_Extern_Visible => "externally_visible"
	| Link_Dllimport      => "dllimport"
	| Link_Dllexport      => "dllexprt"

    fun output linkage = LlvmOutput.str (to_string linkage)
  end

  structure Visibility =
  struct
    (** Describes visibility of linked modules *)
    datatype t = Vis_Default
	       | Vis_Hidden
	       | Vis_Protected

    fun output (vis : t) : LlvmOutput.t =
	let
	  open LlvmOutput
	in
	  str (case vis of
		 Vis_Default   => "default"
	       | Vis_Hidden    => "hidden"
	       | Vis_Protected => "protected")

	end
  end

  structure GarbageCollector =
  struct
    datatype t = GC_OCaml

    fun output (gc : t) : LlvmOutput.t =
	let fun to_string (GC_OCaml) = "ocaml"
        in
	  LlvmOutput.str (to_string gc)
	end
  end

  structure Layout =
  struct
    datatype u = L_Big
	       | L_Little
	       | L_Pointer of size
	       | L_Integer of size
	       | L_Vector  of size
	       | L_Float   of size
	       | L_Aggregate of size
    withtype size = {size: int, abi: int, pref: int option}

    type t = u list

    fun stringify l =
	let
	  fun output_size {size, abi, pref} =
	      String.concat [Int.toString size, ":", Int.toString abi,
			     case pref of
			       NONE => ""
			     | SOME n => ":" ^ Int.toString n]
	in
	  case l of
	    L_Big => "E"
	  | L_Little => "e"
	  | L_Pointer sz => "p" ^ output_size sz
	  | L_Integer sz => "i" ^ output_size sz
	  | L_Vector  sz => "v" ^ output_size sz
	  | L_Float   sz => "f" ^ output_size sz
	  | L_Aggregate sz => "a" ^ output_size sz
	end

    fun output layout_spec =
	let open LlvmOutput in
	  intersperse (str "-") (List.map (fn l => str (stringify l)) layout_spec)
	end
  end

  structure Module =
  struct
    datatype options = Constant
    datatype global =
	     G_Value of {id: Identifier.t,
			 value: Value.t}
	   | G_NamedType of {id: Identifier.t,
			     ty: Type.t}
	   | G_Alias of {id: Identifier.t,
			 linkage: Linkage.t option,
			 visibility: Visibility.t option,
			 alias_ty: Type.t,
			 aliasee: Identifier.t}
	   | G_Layout of Layout.t
	   | G_Decl of {id: Identifier.t,
			linkage: Linkage.t option,
			visibility: Visibility.t option,
			callconv: CallConv.t option,
			ret_attrs: ParamAttr.t list,
			ret_ty: Type.t,
			args: (Type.t * Identifier.t * ParamAttr.t list) list,
			fn_attrs: FunctionAttr.t list,
			section: string option,
			align: Align.t option,
			gc: GarbageCollector.t option}
	   | G_Func of
	     {id: Identifier.t,
	      linkage: Linkage.t option,
	      visibility: Visibility.t option,
	      callconv: CallConv.t option,
	      ret_attrs: ParamAttr.t list,
	      ret_ty: Type.t,
	      args: (Type.t * Identifier.t * ParamAttr.t list) list,
	      fn_attrs: FunctionAttr.t list,
	      section: string option,
	      align: Align.t option,
	      gc: GarbageCollector.t option,
	      body: BasicBlock.t list ref}
    fun mk_func id ret_ty args =
	G_Func { id = id, linkage = NONE, visibility = NONE,
		 callconv = NONE,
		 ret_attrs = [],
		 fn_attrs = [],
		 align = NONE,
		 gc = NONE,
		 section = NONE,
		 ret_ty = ret_ty, args = args, body = ref [] }
    fun bb_push (G_Func { body, ...}) bb =
	body := bb :: (!body)
      | bb_push _ _ = raise
	    (Internal_Error "Trying to push a basic block to a non-function")

    type t = global list

    fun output_global gbl =
	let
	  open LlvmOutput
	  fun output_args args =
	      commas (List.map (fn (ty, id, pattrs) =>
				   seq_space [Type.output ty,
					      seq_space (List.map ParamAttr.output pattrs),
					      Identifier.output id])
			       args)
	in
	  case gbl of
	    G_Value {id, value} =>
	      seq_space [Identifier.output id, str "=", Value.output value]
	  | G_NamedType {id, ty} =>
	      seq_space [Identifier.output id, str "= type", Type.output ty]
	  | G_Alias {id, linkage, visibility, alias_ty, aliasee} =>
	    seq_space [Identifier.output id, str "= alias",
		       case linkage of
			 NONE => null
		       | SOME l => Linkage.output l,
		       case visibility of
			 NONE => null
		       | SOME v => Visibility.output v,
		       Type.output alias_ty,
		       Identifier.output aliasee]
	  | G_Layout l =>
	    seq_space [str "target layout =", Layout.output l]
	  | G_Decl {id, linkage, visibility, callconv, ret_ty, args,
		    ret_attrs, fn_attrs, section, align, gc} =>
	    seq_space [str "define",
		       case linkage of
			 NONE => null
		       | SOME l => Linkage.output l,
		       case visibility of
			 NONE => null
			 | SOME v => Visibility.output v,
		       case callconv of
			 NONE => null
		       | SOME cc => CallConv.output cc,
		       case ret_attrs of
			 [] => null
		       | lst => seq_space (List.map ParamAttr.output lst),
		       Type.output ret_ty,
		       seq [str "@", Identifier.output id],
		       parens (output_args args),
		       case fn_attrs of
			 [] => null
		       | lst => seq_space (List.map FunctionAttr.output lst),
		       case section of
			 NONE => null
		       | SOME name => seq_space [str "section", quoted_str name],
		       case align of
			 NONE => null
		       | SOME n => seq_space [str "align", Align.output n],
		       case gc of
			 NONE => null
		       | SOME gc => seq_space [str "gc", GarbageCollector.output gc]]
	  | G_Func {id, linkage, visibility, callconv, ret_ty, args, body,
		ret_attrs, fn_attrs, section, align, gc} =>
	    let
	      fun output_bodies bodies =
		  seq (List.map BasicBlock.output bodies)
	    in
	      seq_space [str "define",
			 case linkage of
			   NONE => null
			 | SOME l => Linkage.output l,
			 case visibility of
			   NONE => null
			 | SOME v => Visibility.output v,
			 case callconv of
			   NONE => null
			 | SOME cc => CallConv.output cc,
			 case ret_attrs of
			   [] => null
			 | lst => seq_space (List.map ParamAttr.output lst),
			 Type.output ret_ty,
			 seq [str "@", Identifier.output id],
			 parens (output_args args),
			 case fn_attrs of
			   [] => null
			 | lst => seq_space (List.map FunctionAttr.output lst),
			 case section of
			   NONE => null
			 | SOME name => seq_space [str "section", quoted_str name],
			 case align of
			   NONE => null
			 | SOME n => seq_space [str "align", Align.output n],
			 case gc of
			   NONE => null
			 | SOME gc => seq_space [str "gc", GarbageCollector.output gc],
			 braces (output_bodies (!body))]
	    end
	end
  end
end

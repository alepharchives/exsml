/* The bytecode interpreter */

#include <math.h>
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include <stdio.h>

#include "alloc.h"
#include "debugger.h"
#include "fail.h"
#include "instruct.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"
#include "signals.h"
#include "stacks.h"
#include "str.h"
#include "unalignd.h"
#include "interp.h"
#include "global_tbl.h"

#ifdef DEBUG
static long icount = 0;
static void stop_here (void) {}
#endif

/* Registers for the abstract machine:
	pc         the code pointer
	sp         the stack pointer (grows downward)
        accu       the accumulator
        env        heap-allocated environment
	trapsp     pointer to the current trap frame
        extra_args number of extra arguments provided by the caller

sp is a local copy of the global variable extern_sp. */


/* The empty environment */

#define null_env Atom(0)

/* These operations help us read some bytes immediately after the pc, as that is
   the place for extra arguments to opcodes */
#define u8pc  (unsigned char)(*pc)
#define u8pci (unsigned char)(*pc++)
#define JUMPTGT(offset) (bytecode_t)(pc + offset)
#define JUMPSWITCHINDEX(pc, accu) (bytecode_t)(pc + s32(pc + 4 * VAL_TO_LONG(accu)))

/* GC interface */
#define SETUP_FOR_GC { sp -= 2; sp[0] = accu; sp[1] = env; extern_sp = sp; }
#define RESTORE_AFTER_GC { accu = sp[0]; env = sp[1]; sp += 2; }
#define SETUP_FOR_C_CALL { *--sp = env; extern_sp = sp; }
#define RESTORE_AFTER_C_CALL { sp = extern_sp; env = *sp++; }

/* The type of bytecode instructions */

typedef unsigned char opcode_t;

/* byte_raise_break_exn raises the Interrupt exception
   (GETGLOBAL takes a long (4B) arg)

   byte_callback[123]_code do callbacks from C to ML code:
   POP, 1, 0 means pop(1)
*/

#if defined(WORDS_BIGENDIAN) && !defined(HAVE_ALIGNED_ACCESS_REQUIRED)
static opcode_t byte_raise_break_exn[] =
  { GETGLOBAL, 0, 0, 0, EXN_INTERRUPT, RAISE };
static opcode_t byte_callback1_code[] = { ACC1, APPLY1, POP, 0, 1, STOP };
static opcode_t byte_callback2_code[] = { ACC2, APPLY2, POP, 0, 1, STOP };
static opcode_t byte_callback3_code[] = { ACC3, APPLY3, POP, 0, 1, STOP };
#else
static opcode_t byte_raise_break_exn[] =
  { GETGLOBAL, EXN_INTERRUPT, 0, 0, 0, RAISE };
static opcode_t byte_callback1_code[] = { ACC1, APPLY1, POP, 1, 0, STOP };
static opcode_t byte_callback2_code[] = { ACC2, APPLY2, POP, 1, 0, STOP };
static opcode_t byte_callback3_code[] = { ACC3, APPLY3, POP, 1, 0, STOP };
#endif
#define RAISE_CODE_LEN 6
#define CALLBACK_CODE_LEN 6

bytecode_t callback1_code;		/* Set by interprete on initialization */
bytecode_t callback2_code;
bytecode_t callback3_code;


/* The interpreter itself */

extern value interprete(int mode, bytecode_t bprog, bytecode_t* rprog)
{

/*  mode      = mode (0=init, 1=bytecode exec, 2=code exec)
    bprog     = a bytecode array pointer (used in mode 1)
    code_size = the bytecode length
    rprog     = a code pointer pointer (input in mode 2; output in mode 1)
*/

/* Declarations for the registers of the abstract machine.
   The most heavily used registers come first.
*/

	bytecode_t pc = bprog;
	value accu;
	value *sp;

	value env;
	int extra_args;
	struct longjmp_buffer * initial_external_raise;
	int initial_sp_offset;
	value *initial_c_roots_head;
	struct longjmp_buffer raise_buf;
	value *modify_dest, modify_newval;
	value tmp;
	int cur_instr;
	double dtmp;

	switch (mode) {
	case 0:			// initialization
		raise_break_exn = byte_raise_break_exn;
		callback1_code = byte_callback1_code;
		callback2_code = byte_callback2_code;
		callback3_code = byte_callback3_code;
		return Atom(0);
	case 1:			// bytecode execution
		pc = bprog;
		break;
	case 2:			// bytecode execution, used by callback
		pc = *rprog;
		break;
	default:
		perror("Unknown interpretation mode");
		break;
	}

/* To read immediate operands, read some bytes after pc: */
	sp = extern_sp;
	extra_args = 0;
	env = null_env;
	accu = LONG_TO_VAL(0);
	initial_c_roots_head = c_roots_head;
	initial_sp_offset = stack_high - sp;
	initial_external_raise = external_raise;

	if (setjmp(raise_buf.buf)) {
		c_roots_head = initial_c_roots_head;
		accu = exn_bucket;
		goto raise_exception;
	}
	external_raise = &raise_buf;

#ifdef DEBUG
	log_ptr = log_buffer;
#endif

	while (1) {
#ifdef DEBUG
		if (icount-- == 0) {
			stop_here ();
		}

		*log_ptr++ = pc;
		if (log_ptr >= log_buffer + LOG_BUFFER_SIZE) {
			log_ptr = log_buffer;
		}
		assert(sp >= stack_low);
		assert(sp <= stack_high);
		cur_instr = *pc++;
		disasm_instr(cur_instr, pc, accu, sp);
#else
		cur_instr = *pc++;
#endif

		switch (cur_instr) {

/* Basic stack operations */
		case SWAP:
		{
			value tmp2 = accu;
			accu = sp[0];
			sp[0] = tmp2;
			break;
		}

		case PUSH:
//      printf("PUSH\n");
		case PUSHACC0:
			*--sp = accu;
			break;
		case ACC0:
			accu = sp[0];
			break;

		case PUSHACC1:
			*--sp = accu; /* Fallthrough */
		case ACC1:
			accu = sp[1];
			break;

		case PUSHACC2:
			*--sp = accu; /* Fallthrough */
		case ACC2:
			accu = sp[2];
			break;

		case PUSHACC3:
			*--sp = accu; /* Fallthrough */
		case ACC3:
			accu = sp[3];
			break;

		case PUSHACC4:
			*--sp = accu; /* Fallthrough */
		case ACC4:
			accu = sp[4];
			break;

		case PUSHACC5:
			*--sp = accu; /* Fallthrough */
		case ACC5:
			accu = sp[5];
			break;

		case PUSHACC6:
			*--sp = accu; /* Fallthrough */
		case ACC6:
			accu = sp[6];
			break;

		case PUSHACC7:
			*--sp = accu; /* Fallthrough */
		case ACC7:
			accu = sp[7];
			break;

		case PUSHACC:
			*--sp = accu; /* Fallthrough */
		case ACCESS:
			accu = sp[u16(pc)]; pc += sizeof(short);
			break;

		case POP:
			sp += u16(pc); pc += sizeof(short);
			break;
		case ASSIGN:
			sp[u16(pc)] = accu; pc += sizeof(short);
			accu = Val_unit;
			break;

/* Access in heap-allocated environment */

		case PUSHENV1:
			*--sp = accu; /* Fallthrough */
		case ENV1:
			accu = Field(env, 1);
			break;

		case PUSHENV2:
			*--sp = accu; /* Fallthrough */
		case ENV2:
			accu = Field(env, 2);
			break;

		case PUSHENV3:
			*--sp = accu; /* Fallthrough */
		case ENV3:
			accu = Field(env, 3);
			break;

		case PUSHENV4:
			*--sp = accu; /* Fallthrough */
		case ENV4:
			accu = Field(env, 4);
			break;

		case PUSHENV5:
			*--sp = accu; /* Fallthrough */
		case ENV5:
			accu = Field(env, 5);
			break;

		case PUSHENV6:
			*--sp = accu; /* Fallthrough */
		case ENV6:
			accu = Field(env, 6);
			break;

		case PUSHENV7:
			*--sp = accu; /* Fallthrough */
		case ENV7:
			accu = Field(env, 7);
			break;

		case PUSHENVACC:
			*--sp = accu; /* Fallthrough */
		case ENVACC:
			accu = Field(env, u16(pc));
			pc += sizeof(short);
			break;

		case PUSH_ENV1_APPLY1:
		{
			sp -= 4;
			sp[0] = accu;
			sp[1] = (value)pc;
			sp[2] = env;
			sp[3] = LONG_TO_VAL(extra_args);
			extra_args = 0;
			accu = Field(env, 1);
			pc = Code_val(accu);
			env = accu;
			goto stack_check;
		}

		case PUSH_ENV1_APPLY2:
		{
			value arg2 = sp[0];
			sp -= 4;
			sp[0] = accu;
			sp[1] = arg2;
			sp[2] = (value)pc;
			sp[3] = env;
			sp[4] = LONG_TO_VAL(extra_args);
			extra_args = 1;
			accu = Field(env, 1);
			pc = Code_val(accu);
			env = accu;
			goto stack_check;
		}

		case PUSH_ENV1_APPLY3:
		{
			value arg2 = sp[0];
			value arg3 = sp[1];
			sp -= 4;
			sp[0] = accu;
			sp[1] = arg2;
			sp[2] = arg3;
			sp[3] = (value)pc;
			sp[4] = env;
			sp[5] = LONG_TO_VAL(extra_args);
			extra_args = 2;
			accu = Field(env, 1);
			pc = Code_val(accu);
			env = accu;
			goto stack_check;
		}

		case PUSH_ENV1_APPLY4:
		{
			value arg2 = sp[0];
			value arg3 = sp[1];
			value arg4 = sp[2];
			sp -= 4;
			sp[0] = accu;
			sp[1] = arg2;
			sp[2] = arg3;
			sp[3] = arg4;
			sp[4] = (value)pc;
			sp[5] = env;
			sp[6] = LONG_TO_VAL(extra_args);
			extra_args = 3;
			accu = Field(env, 1);
			pc = Code_val(accu);
			env = accu;
			goto stack_check;
		}

		case PUSH_ENV1_APPTERM1:
		{
			sp = sp + u16(pc) - 2;
			pc += sizeof(short);

			sp[0] = accu;
		} /* Fall through */
		env1_appterm:
		        accu = Field(env, 1);
		appterm:
			pc = Code_val(accu);
			env = accu;
			goto check_signals;

		case PUSH_ENV1_APPTERM2:
		{
			value arg2 = sp[0];
			sp = sp + u16(pc) - 3; pc += sizeof(short);
			sp[0] = accu;
			sp[1] = arg2;
			extra_args += 1;
			goto env1_appterm;
		}

		case PUSH_ENV1_APPTERM3:
		{
			value arg2 = sp[0];
			value arg3 = sp[1];
			sp = sp + u16(pc) - 4; pc += sizeof(short);
			sp[0] = accu;
			sp[1] = arg2;
			sp[2] = arg3;
			extra_args += 2;
			goto env1_appterm;
		}

		case PUSH_ENV1_APPTERM4:
		{
			value arg2 = sp[0];
			value arg3 = sp[1];
			value arg4 = sp[2];
			sp = sp + u16(pc) - 5; pc += sizeof(short);
			sp[0] = accu;
			sp[1] = arg2;
			sp[2] = arg3;
			sp[3] = arg4;
			extra_args += 3;
			goto env1_appterm;
		}

/* Function application */

		case PUSH_RETADDR: {
			sp -= 3;
			sp[0] = (value) (JUMPTGT(s32(pc)));
			sp[1] = env;
			sp[2] = LONG_TO_VAL(extra_args);
			pc += sizeof(int32_t);
			break;
		}

		case APPLY: {
			extra_args = u8pc - 1;
			pc = Code_val(accu);
			env = accu;
			goto stack_check;
		}

		case APPLY1: {
			value arg1 = sp[0];
			sp -= 3;
			sp[0] = arg1;
			sp[1] = (value)pc;
			sp[2] = env;
			sp[3] = LONG_TO_VAL(extra_args);
			extra_args = 0;
			pc = Code_val(accu);
			env = accu;
			goto stack_check;
		}

		case APPLY2: {
			value arg1 = sp[0];
			value arg2 = sp[1];
			sp -= 3;
			sp[0] = arg1;
			sp[1] = arg2;
			sp[2] = (value)pc;
			sp[3] = env;
			sp[4] = LONG_TO_VAL(extra_args);
			extra_args = 1;
			pc = Code_val(accu);
			env = accu;
			goto stack_check;
		}

		case APPLY3: {
			value arg1 = sp[0];
			value arg2 = sp[1];
			value arg3 = sp[2];
			sp -= 3;
			sp[0] = arg1;
			sp[1] = arg2;
			sp[2] = arg3;
			sp[3] = (value)pc;
			sp[4] = env;
			sp[5] = LONG_TO_VAL(extra_args);
			extra_args = 2;
			pc = Code_val(accu);
			env = accu;
			goto stack_check;
		}

		case APPLY4: {
			value arg1 = sp[0];
			value arg2 = sp[1];
			value arg3 = sp[2];
			value arg4 = sp[3];
			sp -= 3;
			sp[0] = arg1;
			sp[1] = arg2;
			sp[2] = arg3;
			sp[3] = arg4;
			sp[4] = (value)pc;
			sp[5] = env;
			sp[6] = LONG_TO_VAL(extra_args);
			extra_args = 3;
			pc = Code_val(accu);
			env = accu;
			goto stack_check;
		}

		case APPTERM: {
			int nargs = u8pci;
			int slotsize = u16(pc);
			value * newsp;
			int i;
			pc += sizeof(short);
			/* Slide the nargs bottom words of the current frame to the top
			   of the frame, and discard the remainder of the frame */
			newsp = sp + slotsize - nargs;
			for (i = nargs - 1; i >= 0; i--) {
				newsp[i] = sp[i];
			}
			sp = newsp;
			extra_args += nargs - 1;
			goto appterm;
		}

		case APPTERM1: {
			value arg1 = sp[0];
			sp = sp + u16(pc) - 1; pc += sizeof(short);
			sp[0] = arg1;
			goto appterm;
		}

		case APPTERM2: {
			value arg1 = sp[0];
			value arg2 = sp[1];
			sp = sp + u16(pc) - 2; pc += sizeof(short);
			sp[0] = arg1;
			sp[1] = arg2;
			extra_args += 1;
			goto appterm;
		}

		case APPTERM3: {
			value arg1 = sp[0];
			value arg2 = sp[1];
			value arg3 = sp[2];
			sp = sp + u16(pc) - 3; pc += sizeof(short);
			sp[0] = arg1;
			sp[1] = arg2;
			sp[2] = arg3;
			extra_args += 2;
			goto appterm;
		}

		case APPTERM4: {
			value arg1 = sp[0];
			value arg2 = sp[1];
			value arg3 = sp[2];
			value arg4 = sp[3];
			sp = sp + u16(pc) - 4; pc += sizeof(short);
			sp[0] = arg1;
			sp[1] = arg2;
			sp[2] = arg3;
			sp[3] = arg4;
			extra_args += 3;
			goto appterm;
		}

		case RETURN1:
			sp += 1;
			goto return_code;

		case RETURN2:
			sp += 2;
			goto return_code;

		case RETURN:
			sp += u16(pc); pc += sizeof(short);
		return_code:
			if (extra_args > 0) {
				extra_args--;
				pc = Code_val(accu);
				env = accu;
			} else {
				pc = (bytecode_t)(sp[0]);
				env = sp[1];
				extra_args = VAL_TO_LONG(sp[2]);
				sp += 3;
				if (something_to_do) {
					goto process_signal;
				}
			}
			break;

		case RESTART: {
			int num_args = Wosize_val(env) - 2;
			int i;
			sp -= num_args;
			for (i = 0; i < num_args; i++) sp[i] = Field(env, i + 2);
			env = Field(env, 1);
			extra_args += num_args;
			break;
		}

		case GRAB: {
			int required = u8pci;
			if (extra_args >= required) {
				extra_args -= required;
			} else {
				mlsize_t num_args, i;
				num_args = 1 + extra_args; /* arg1 + extra args */
				ALLOC_SMALL(accu, num_args + 2, Closure_tag);
				Field(accu, 1) = env;
				for (i = 0; i < num_args; i++) {
					Field(accu, i + 2) = sp[i];
				}
				/* Point to the preceding RESTART instruction.  This works in the
				   bytecode as well as the threaded code; in both cases we have
				   three slots: RESTART, GRAB, n; and pc pointing past n now. */
				Code_val(accu) = pc - 3;
				sp += num_args;
				pc = (bytecode_t)(sp[0]);
				env = sp[1];
				extra_args = VAL_TO_LONG(sp[2]);
				sp += 3;
			}
			break;
		}

		case CLOSURE: {
			int nvars = u8pci;
			int i;
			if (nvars > 0) {
				*--sp = accu;
			}

			ALLOC_SMALL(accu, 1 + nvars, Closure_tag);
			//      printf("pc = %d, s32(pc) = %d\n", pc, s32(pc));
			Code_val(accu) = JUMPTGT(s32(pc));
			//      printf("CLOSURE Code_val(%d) = %d\n", accu, Code_val(accu));
			for (i = 0; i < nvars; i++) {
				Field(accu, i + 1) = sp[i];
			}
			sp += nvars;
			pc += sizeof(int32_t);
			break;
		}

		case CLOSREC:
		{
			int nvars = u8pci;
			int i;
			if (nvars > 0) *--sp = accu;
			ALLOC_SMALL(accu, 2 + nvars, Closure_tag);
			Code_val(accu) = JUMPTGT(s32(pc));
			//      printf("CLOSREC Code_val(%d) = %d\n", accu, Code_val(accu));
			Field(accu, 1) = INT_TO_VAL(0);
			for (i = 0; i < nvars; i++) Field(accu, i + 2) = sp[i];
			sp += nvars;
			modify(&Field(accu, 1), accu);
			pc += sizeof(int32_t);
			break;
		}

/* For recursive definitions */

		case DUMMY: {
			int size = u16(pc) + 1; /* size + 1 to match CLOSURE */
			pc += sizeof(short);
			ALLOC_SMALL(accu, size, 0);
			while (size--) {
				Field(accu, size) = LONG_TO_VAL(0);
			}
			break;
		}

		case UPDATE: {
			value newval = *sp++;
			mlsize_t size, n;
			size = Wosize_val(newval);
			assert(size == Wosize_val(accu));
			Tag_val(accu) = Tag_val(newval);
			for (n = 0; n < size; n++) {
				modify(&Field(accu, n), Field(newval, n));
			}
			accu = Val_unit;
			break;
		}

/* Globals */

		case PUSH_GETGLOBAL:
			*--sp = accu;
			/* Fallthrough */
		case GETGLOBAL:
			accu = Field(global_data, u32(pc));
			pc += sizeof(uint32_t);
			break;

		case PUSH_GETGLOBAL_APPLY1:
		{
			sp -= 4;
			sp[0] = accu;
			accu = Field(global_data, u32(pc));
			pc += sizeof(uint32_t);
			sp[1] = (value)pc;
			sp[2] = env;
			sp[3] = LONG_TO_VAL(extra_args);
			extra_args = 0;
			pc = Code_val(accu);
			env = accu;
		}
		/* Fall through to stack check */
		stack_check:
		if (sp < stack_threshold) {
			extern_sp = sp;
			realloc_stack();
			sp = extern_sp;
		}
		/* Fall through to signals check */

		check_signals:

		case CHECK_SIGNALS:    /* accu not preserved */
			if (something_to_do) {
				goto process_signal;
			}
			break;

		case PUSH_GETGLOBAL_APPLY2:
		{
			value arg2 = sp[0];
			sp -= 4;
			sp[0] = accu;
			sp[1] = arg2;
			accu = Field(global_data, u32(pc));
			pc += sizeof(uint32_t);
			sp[2] = (value)pc;
			sp[3] = env;
			sp[4] = LONG_TO_VAL(extra_args);
			extra_args = 1;
			pc = Code_val(accu);
			env = accu;
			goto stack_check;
		}

		case PUSH_GETGLOBAL_APPLY3:
		{
			value arg2 = sp[0];
			value arg3 = sp[1];
			sp -= 4;
			sp[0] = accu;
			sp[1] = arg2;
			sp[2] = arg3;
			accu = Field(global_data, u32(pc));
			pc += sizeof(uint32_t);
			sp[3] = (value)pc;
			sp[4] = env;
			sp[5] = LONG_TO_VAL(extra_args);
			extra_args = 2;
			pc = Code_val(accu);
			env = accu;
			goto stack_check;
		}

		case PUSH_GETGLOBAL_APPLY4:
		{
			value arg2 = sp[0];
			value arg3 = sp[1];
			value arg4 = sp[2];
			sp -= 4;
			sp[0] = accu;
			sp[1] = arg2;
			sp[2] = arg3;
			sp[3] = arg4;
			accu = Field(global_data, u32(pc));
			pc += sizeof(uint32_t);
			sp[4] = (value)pc;
			sp[5] = env;
			sp[6] = LONG_TO_VAL(extra_args);
			extra_args = 3;
			pc = Code_val(accu);
			env = accu;
			goto stack_check;
		}

		case PUSH_GETGLOBAL_APPTERM1:
			/* opcode, popnbr, globalindex */
			sp = sp + u16(pc) - 2; pc += sizeof(short);
			sp[0] = accu;
		getglobal_appterm:
			accu = Field(global_data, u32(pc));
			pc = Code_val(accu);
			env = accu;
			goto check_signals;

		case PUSH_GETGLOBAL_APPTERM2:
		{
			value arg2 = sp[0];
			sp = sp + u16(pc) - 3; pc += sizeof(short);
			sp[0] = accu;
			sp[1] = arg2;
			extra_args += 1;
			goto getglobal_appterm;
		}

		case PUSH_GETGLOBAL_APPTERM3:
		{
			value arg2 = sp[0];
			value arg3 = sp[1];
			sp = sp + u16(pc) - 4; pc += sizeof(short);
			sp[0] = accu;
			sp[1] = arg2;
			sp[2] = arg3;
			extra_args += 2;
			goto getglobal_appterm;
		}

		case PUSH_GETGLOBAL_APPTERM4:
		{
			value arg2 = sp[0];
			value arg3 = sp[1];
			value arg4 = sp[2];
			sp = sp + u16(pc) - 5; pc += sizeof(short);
			sp[0] = accu;
			sp[1] = arg2;
			sp[2] = arg3;
			sp[3] = arg4;
			extra_args += 3;
			goto getglobal_appterm;
		}

		case SETGLOBAL:
			modify(&Field(global_data, u32(pc)), accu);
			accu = Val_unit; /* ? */
			pc += sizeof(uint32_t);
			break;

/* Allocation of blocks */

		case PUSHATOM0:
			*--sp = accu;
			/* Fallthrough */
		case ATOM0:
			accu = Atom(0); break;

		case ATOM1:
			accu = Atom(1);
			break;
		case ATOM2:
			accu = Atom(2);
			break;
		case ATOM3:
			accu = Atom(3);
			break;
		case ATOM4:
			accu = Atom(4);
			break;
		case ATOM5:
			accu = Atom(5);
			break;
		case ATOM6:
			accu = Atom(6);
			break;
		case ATOM7:
			accu = Atom(7);
			break;
		case ATOM8:
			accu = Atom(8);
			break;
		case ATOM9:
			accu = Atom(9);
			break;

		case PUSHATOM:
			*--sp = accu;
			/* Fallthrough */
		case ATOM:
			accu = Atom(u8pci); break;

		case MAKEBLOCK:
		{
			header_t hdr;
			mlsize_t size;
			tag_t tag;
			int i;

			hdr = u32(pc);
			pc += sizeof(int32_t);
			size = Wosize_hd(hdr);
			tag = Tag_hd(hdr);
			if (size < Max_young_wosize) {
				ALLOC_SMALL(tmp, size, tag);
				Field(tmp, size-1) = accu;
				for (i = size-2; i >= 0; i--) Field(tmp, i) = *sp++;
				accu = tmp;
			} else {
				SETUP_FOR_GC;
				tmp = alloc_shr (size, tag);
				RESTORE_AFTER_GC;
				initialize (&Field(tmp, size-1), accu);
				for (i = size-2; i >= 0; i--) initialize (&Field(tmp, i), *sp++);
				accu = tmp;
			}
			break;
		}

		case MAKEBLOCK1: {
			tag_t tag = u8pci;
			value block;
			ALLOC_SMALL(block, 1, tag);
			Field(block, 0) = accu;
			accu = block;
			break;
		}

		case MAKEBLOCK2: {
			tag_t tag = u8pci;
			value block;
			ALLOC_SMALL(block, 2, tag);
			Field(block, 0) = sp[0];
			Field(block, 1) = accu;
			sp += 1;
			accu = block;
			break;
		}

		case MAKEBLOCK3: {
			tag_t tag = u8pci;
			value block;
			ALLOC_SMALL(block, 3, tag);
			Field(block, 0) = sp[1];
			Field(block, 1) = sp[0];
			Field(block, 2) = accu;
			sp += 2;
			accu = block;
			break;
		}

		case MAKEBLOCK4: {
			tag_t tag = u8pci;
			value block;
			ALLOC_SMALL(block, 4, tag);
			Field(block, 0) = sp[2];
			Field(block, 1) = sp[1];
			Field(block, 2) = sp[0];
			Field(block, 3) = accu;
			sp += 3;
			accu = block;
			break;
		}

/* Access to components of blocks */

		case GETFIELD0:
			accu = Field(accu, 0);
			break;
		case GETFIELD1:
			accu = Field(accu, 1);
			break;
		case GETFIELD2:
			accu = Field(accu, 2);
			break;
		case GETFIELD3:
			accu = Field(accu, 3);
			break;
		case GETFIELD:
			accu = Field(accu, u16(pc));
			pc += sizeof(short);
			break;

		case GETFIELD0_0:
			accu = Field(accu, 0);
			accu = Field(accu, 0);
			break;

		case GETFIELD0_1:
			accu = Field(accu, 0);
			accu = Field(accu, 1);
			break;

		case GETFIELD1_0:
			accu = Field(accu, 1);
			accu = Field(accu, 0);
			break;

		case GETFIELD1_1:
			accu = Field(accu, 1);
			accu = Field(accu, 1);
			break;

		case SETFIELD0:
			modify_dest = &Field(*sp++, 0);
			modify_newval = accu;
		modify:
			modify(modify_dest, modify_newval);
			accu = Val_unit; /* Atom(0); */
			break;
		case SETFIELD1:
			modify_dest = &Field(*sp++, 1);
			modify_newval = accu;
			goto modify;
		case SETFIELD2:
			modify_dest = &Field(*sp++, 2);
			modify_newval = accu;
			goto modify;
		case SETFIELD3:
			modify_dest = &Field(*sp++, 3);
			modify_newval = accu;
			goto modify;
		case SETFIELD:
			modify_dest = &Field(*sp++, u16(pc));
			pc += sizeof(short);
			modify_newval = accu;
			goto modify;

/* Array operations */

		case VECTLENGTH:
			accu = LONG_TO_VAL(Wosize_val(accu));
			break;
		case GETVECTITEM:
			accu = Field(sp[0], VAL_TO_LONG(accu));
			sp += 1;
			break;
		case SETVECTITEM:
			modify_dest = &Field(sp[1], VAL_TO_LONG(sp[0]));
			modify_newval = accu;
			sp += 2;
			goto modify;

/* String operations */

		case GETSTRINGCHAR:
			accu = INT_TO_VAL(Byte_u(sp[0], VAL_TO_LONG(accu)));
			sp += 1;
			break;
		case SETSTRINGCHAR:
			Byte_u(sp[1], VAL_TO_LONG(sp[0])) = VAL_TO_INT(accu);
			accu = Atom(0);
			sp += 2;
			break;

/* Branches and conditional branches */

		case BRANCH:
//      printf("BRANCH to %d\n", (void**)(s32(pc))-realcode);
			pc = JUMPTGT(s32(pc));
			break;
		case BRANCHIF:
			if (Tag_val(accu) != 0) {
				pc = JUMPTGT(s32(pc));
			} else {
				pc += sizeof(int32_t);
			}
			break;
		case BRANCHIFNOT:
			if (Tag_val(accu) == 0) {
				pc = JUMPTGT(s32(pc));
			} else {
				pc += sizeof(int32_t);
			}
			break;
		case POPBRANCHIFNOT:
			tmp = accu;
			accu = *sp++;
			if (Tag_val(tmp) == 0) {
				pc = JUMPTGT(s32(pc));
			} else {
				pc += sizeof(int32_t);
			}
			break;
		case BRANCHIFNEQTAG:
			if (Tag_val(accu) != u8pci) {
				pc = JUMPTGT(s32(pc));
			} else {
				pc += sizeof(int32_t);
			}
			break;
		case SWITCH:
			assert(VAL_TO_LONG(accu) >= 0 && VAL_TO_LONG(accu) < *pc);
			pc++;
			//      printf("SWITCH: JUMPSWITCHINDEX(pc, %d) = %d\n", accu, JUMPSWITCHINDEX(pc, accu));
			pc = JUMPSWITCHINDEX(pc, accu);
			break;
		case BOOLNOT:
			accu = Atom(Tag_val(accu) == 0); break;

/* Exceptions */

		case PUSHTRAP:
			sp -= 4;
			Trap_pc(sp) = JUMPTGT(s32(pc));
			Trap_link(sp) = trapsp;
			sp[2] = env;
			sp[3] = LONG_TO_VAL(extra_args);
			trapsp = sp;
			pc += sizeof(int32_t);
			break;

		case POPTRAP:
			/*
			 * We should check here if a signal is pending, to preserve the
			 * semantics of the program w.r.t. exceptions. Unfortunately,
			 * process_signal destroys the accumulator, and there is no
			 * convenient way to preserve it...
			 */
			trapsp = Trap_link(sp);
			sp += 4;
			break;

		raise_exception:			/* An external raise jumps here */

		case RAISE:            /* arg */
			sp = trapsp;
			if (sp >= stack_high - initial_sp_offset) {
				exn_bucket = accu;
				external_raise = initial_external_raise;
				longjmp(external_raise->buf, 1);
			}

			pc = Trap_pc(sp);
			trapsp = Trap_link(sp);
			env = sp[2];
			extra_args = VAL_TO_LONG(sp[3]);
			sp += 4;
			break;

		process_signal:
			something_to_do = 0;
			if (force_minor_flag){
				force_minor_flag = 0;
				SETUP_FOR_GC;
				minor_collection ();
				RESTORE_AFTER_GC;
			}

			/* If a signal arrives between the following two instructions,
			   it will be lost. */
			{
				int signal_number2 = signal_is_pending;
				signal_is_pending = 0;
				if (signal_number2) {
					/* Push a return frame to the current code location */
					sp -= 4;
					sp[0] = INT_TO_VAL(signal_number2);
					sp[1] = (value) pc;
					sp[2] = env;
					sp[3] = LONG_TO_VAL(extra_args);
					/* Branch to the signal handler */
					/* e -- signal_handler should be a closure, but isn't in 1.31.
					   env = (value )signal_handler;  // env = Field(signal_handlers, signal_number);
					   pc = Code_val(env);
					   I'm lazy, so for now... */
					env = null_env;
					pc = signal_handler;
					/* */
					extra_args = 0;
				}
			}
			break;

/* Calling C functions */

		case C_CALL1:
			SETUP_FOR_C_CALL;
			accu = (cprim[u16(pc)])(accu);
			RESTORE_AFTER_C_CALL;
			pc += sizeof(short);
			break;
		case C_CALL2:
			SETUP_FOR_C_CALL;
			/* sp[0] temporarily holds the environment pointer */
			accu = (cprim[u16(pc)])(sp[1], accu);
			RESTORE_AFTER_C_CALL;
			pc += sizeof(short);
			sp += 1;
			break;
		case C_CALL3:
			SETUP_FOR_C_CALL;
			accu = (cprim[u16(pc)])(sp[2], sp[1], accu);
			RESTORE_AFTER_C_CALL;
			pc += sizeof(short);
			sp += 2;
			break;
		case C_CALL4:
			SETUP_FOR_C_CALL;
			accu = (cprim[u16(pc)])(sp[3], sp[2], sp[1], accu);
			RESTORE_AFTER_C_CALL;
			pc += sizeof(short);
			sp += 3;
			break;
		case C_CALL5:
			SETUP_FOR_C_CALL;
			accu = (cprim[u16(pc)])(sp[4], sp[3], sp[2], sp[1], accu);
			RESTORE_AFTER_C_CALL;
			pc += sizeof(short);
			sp += 4;
			break;
		case C_CALLN:
		{
			int n = u8pci;
			value * args;
			int i;
			*--sp = accu;
			SETUP_FOR_C_CALL;
			args = (value*)malloc(n * sizeof(value));
			for (i = 0; i < n; i++) {
				args[i] = sp[n-i];
			}
			accu = (cprim[u16(pc)])(args, n);
			RESTORE_AFTER_C_CALL;
			pc += sizeof(short);
			free(args);
			sp += n;
			break;
		}

/* small values */

		case CONSTBYTE:
			accu = u8pci;
			break;

		case CONSTSHORT:
			accu = s16(pc);
			pc += sizeof(short);
			break;

/* Integer constants */

		case PUSHCONST0:
			*--sp = accu; /* Fallthrough */
		case CONST0:
			accu = INT_TO_VAL(0);
			break;

		case PUSHCONST1:
			*--sp = accu; /* Fallthrough */
		case CONST1:
			accu = INT_TO_VAL(1);
			break;

		case PUSHCONST2:
			*--sp = accu; /* Fallthrough */
		case CONST2:
			accu = INT_TO_VAL(2);
			break;

		case PUSHCONST3:
			*--sp = accu; /* Fallthrough */
		case CONST3:
			accu = INT_TO_VAL(3);
			break;

		case PUSHCONSTINT:
			*--sp = accu; /* Fallthrough */
		case CONSTINT:
			accu = INT_TO_VAL(s32(pc));
			pc += sizeof(int32_t);
			break;

/* Unsigned integer arithmetic modulo 2^(wordsize-1) */

		case ADDINT:		/* Modified for Moscow ML: unsigned */
			accu = (unsigned long) ((unsigned long) *sp++
						+ (unsigned long) (accu - 1));
			break;
		case SUBINT:		/* unsigned */
			accu = (unsigned long) ((unsigned long) *sp++
						- (unsigned long) (accu - 1));
			break;
		case MULINT:		/* unsigned */
			accu = (unsigned long) (1 + (unsigned long) (*sp++ >> 1)
						* (unsigned long) (accu - 1));
			break;
		case DIVINT:		/* unsigned */
			tmp = accu - 1;
			if (tmp == 0) {
				accu = Field(global_data, EXN_DIV);
				goto raise_exception;
			}

			accu = LONG_TO_VAL((unsigned long) ((unsigned long) (*sp++ - 1)
							    / (unsigned long) tmp));
			break;

		case MODINT:
			tmp = accu - 1;
			if (tmp == 0) {
				accu = Field(global_data, EXN_DIV);
				goto raise_exception;
			}

			accu = (unsigned long) (1 + (unsigned long) (*sp++ - 1)
						% (unsigned long) tmp);
			break;

		case ANDINT:
			accu &= *sp++;
			break;
		case ORINT:
			accu |= *sp++;
			break;
		case XORINT:
			accu = 1 + (accu ^ *sp++);
			break;
		case SHIFTLEFTINT:
			accu = 1 + ((*sp++ - 1) << VAL_TO_LONG(accu));
			break;
		case SHIFTRIGHTINTSIGNED:
			accu = 1 | ((*sp++ - 1) >> VAL_TO_LONG(accu));
			break;
		case SHIFTRIGHTINTUNSIGNED:
			accu = 1 | ((unsigned long)(*sp++ - 1) >> VAL_TO_LONG(accu));
			break;
		case EQ:
			accu = Atom(*sp++ == accu);
			break;
		case BRANCHIFEQ:
			if (*sp++ == accu) {
				pc = JUMPTGT(s32(pc));
			} else {
				pc += sizeof(int32_t);
			}
			break;
		case NEQ:
			accu = Atom(*sp++ != accu);
			break;
		case BRANCHIFNEQ:
			if (*sp++ != accu) {
				pc = JUMPTGT(s32(pc));
			} else {
				pc += sizeof(int32_t);
			}
			break;
		case LTINT:
			accu = Atom(*sp++ < accu);
			break;
		case BRANCHIFLT:
			if (*sp++ < accu) {
				pc = JUMPTGT(s32(pc));
			} else {
				pc += sizeof(int32_t);
			}
			break;
		case GTINT:
			accu = Atom(*sp++ > accu);
			break;
		case BRANCHIFGT:
			if (*sp++ > accu) {
				pc = JUMPTGT(s32(pc));
			} else {
				pc += sizeof(int32_t);
			}
			break;
		case LEINT:
			accu = Atom(*sp++ <= accu);
			break;
		case BRANCHIFLE:
			if (*sp++ <= accu) {
				pc = JUMPTGT(s32(pc));
			} else {
				pc += sizeof(int32_t);
			}
			break;
		case GEINT:
			accu = Atom(*sp++ >= accu);
			break;
		case BRANCHIFGE:
			if (*sp++ >= accu) {
				pc = JUMPTGT(s32(pc));
			} else {
				pc += sizeof(int32_t);
			}
			break;
		case TAGOF:
			accu = LONG_TO_VAL(Tag_val(accu));
			break;
		case EQUNSIGN:
			accu = (((value) (((header_t *) (&(first_atoms [(unsigned long)(*sp++) == (unsigned long)accu]))) + 1)));
			break;
		case NEQUNSIGN:
			accu = (((value) (((header_t *) (&(first_atoms [(unsigned long)(*sp++) != (unsigned long)accu]))) + 1)));
			break;
		case LTUNSIGN:
			accu = (((value) (((header_t *) (&(first_atoms [(unsigned long)(*sp++) < (unsigned long)accu]))) + 1)));
			break;
		case GTUNSIGN:
			accu = (((value) (((header_t *) (&(first_atoms [(unsigned long)(*sp++) > (unsigned long)accu]))) + 1)));
			break;
		case LEUNSIGN:
			accu = (((value) (((header_t *) (&(first_atoms [(unsigned long)(*sp++) <= (unsigned long)accu]))) + 1)));
			break;
		case GEUNSIGN:
			accu = (((value) (((header_t *) (&(first_atoms [(unsigned long)(*sp++) >= (unsigned long)accu]))) + 1)));
			break;
		case BRANCHINTERVAL:
		{
			value low_bound, high_bound;
			high_bound = accu;
			low_bound = *sp++;
			accu = *sp++;
			if (accu < low_bound) {
				pc = JUMPTGT(s32(pc));
				break;
			}
			pc += sizeof(int32_t);
			if (accu > high_bound) {
				pc = JUMPTGT(s32(pc));
				break;
			}
			pc += sizeof(int32_t);
			accu = accu - low_bound + 1;
			break;
		}

		/* --- Moscow SML changes begin --- */

#define Check_float(dval)						\
		if ((dval > maxdouble) || (dval < -maxdouble))		\
		{ accu = Field(global_data, EXN_OVERFLOW); goto raise_exception; }

		case FLOATOFINT:
			dtmp = (double) VAL_TO_LONG(accu);
			goto float_done;

		case SMLNEGFLOAT:
			dtmp = -Double_val(accu);
			Check_float(dtmp);
			goto float_done;

		case SMLADDFLOAT:
			dtmp = Double_val(*sp++) + Double_val(accu);
			Check_float(dtmp);
			goto float_done;

		case SMLSUBFLOAT:
			dtmp = Double_val(*sp++) - Double_val(accu);
			Check_float(dtmp);
			goto float_done;

		case SMLMULFLOAT:
			dtmp = Double_val(*sp++) * Double_val(accu);
			Check_float(dtmp);
			goto float_done;

		case SMLDIVFLOAT:
			dtmp = Double_val(accu);
			if (dtmp == 0.0) {
				accu = Field(global_data, EXN_DIV);
				goto raise_exception;
			}
			dtmp = Double_val(*sp++) / dtmp;
			Check_float(dtmp); /* Fallthrough */
		float_done:
			ALLOC_SMALL(tmp, Double_wosize, Double_tag);
			Store_double_val(tmp, dtmp);
			accu = tmp;
			break;

		/* --- Moscow SML changes end --- */

		case INTOFFLOAT:
			accu = LONG_TO_VAL((long)Double_val(accu));
			break;

		case EQFLOAT:
			accu = (((value) (((header_t *) (&(first_atoms [(* (double *) (*sp++)) == (* (double *) (accu))]))) + 1)));
			break;
		case NEQFLOAT:
			accu = (((value) (((header_t *) (&(first_atoms [(* (double *) (*sp++)) != (* (double *) (accu))]))) + 1)));
			break;
		case LTFLOAT:
			accu = (((value) (((header_t *) (&(first_atoms [(* (double *) (*sp++)) < (* (double *) (accu))]))) + 1)));
			break;
		case GTFLOAT:
			accu = (((value) (((header_t *) (&(first_atoms [(* (double *) (*sp++)) > (* (double *) (accu))]))) + 1)));
			break;
		case LEFLOAT:
			accu = (((value) (((header_t *) (&(first_atoms [(* (double *) (*sp++)) <= (* (double *) (accu))]))) + 1)));
			break;
		case GEFLOAT:
			accu = (((value) (((header_t *) (&(first_atoms [(* (double *) (*sp++)) >= (* (double *) (accu))]))) + 1)));
			break;
		case STRINGLENGTH:
			accu = LONG_TO_VAL(string_length(accu));
			break;
		case EQSTRING:
			accu = (((value) (((header_t *) (&(first_atoms [compare_strings(*sp++, accu) == (((long)(0) << 1) + 1)]))) + 1)));
			break;
		case NEQSTRING:
			accu = (((value) (((header_t *) (&(first_atoms [compare_strings(*sp++, accu) != (((long)(0) << 1) + 1)]))) + 1)));
			break;
		case LTSTRING:
			accu = (((value) (((header_t *) (&(first_atoms [compare_strings(*sp++, accu) < (((long)(0) << 1) + 1)]))) + 1)));
			break;
		case GTSTRING:
			accu = (((value) (((header_t *) (&(first_atoms [compare_strings(*sp++, accu) > (((long)(0) << 1) + 1)]))) + 1)));
			break;
		case LESTRING:
			accu = (((value) (((header_t *) (&(first_atoms [compare_strings(*sp++, accu) <= (((long)(0) << 1) + 1)]))) + 1)));
			break;
		case GESTRING:
			accu = (((value) (((header_t *) (&(first_atoms [compare_strings(*sp++, accu) >= (((long)(0) << 1) + 1)]))) + 1)));
			break;

		case MAKEVECTOR:
		{
			mlsize_t size = VAL_TO_LONG(sp[0]);
			/* Make sure that the object referred to by sp[0] survives gc: */
			sp[0] = accu;
			if (size == 0) {
				accu = Atom(0);
			} else if (size < Max_young_wosize) {
				ALLOC_SMALL (accu, size, 0);
				do {size--; Field (accu, size) = *sp;} while (size != 0);
			} else if (IS_BLOCK(*sp) && Is_young (*sp)) {
				SETUP_FOR_GC;
				minor_collection ();
				tmp = alloc_shr (size, 0);
				RESTORE_AFTER_GC;
				accu = tmp;
				do {
					size--; Field (accu, size) = *sp;
				} while (size != 0);
			} else {
				SETUP_FOR_GC;
				tmp = alloc_shr (size, 0);
				RESTORE_AFTER_GC;
				accu = tmp;
				do {
					size--; initialize(&Field(accu, size), *sp);
				} while (size != 0);
			}
			sp++;
			break;
		}

/* --- Additional instructions for Moscow SML --- */

		case SMLNEGINT:
			tmp =  - VAL_TO_LONG(accu);
			accu = LONG_TO_VAL(tmp);
			if (VAL_TO_LONG(accu) != tmp) {
				goto raise_overflow;
			}
			break;
		raise_overflow:
			accu = Field(global_data, EXN_OVERFLOW);
			goto raise_exception;

		case SMLSUCCINT:
			tmp =  VAL_TO_LONG(accu) + 1;
			accu = LONG_TO_VAL(tmp);
			if (VAL_TO_LONG(accu) != tmp) {
				goto raise_overflow;
			}
			break;
		case SMLPREDINT:
			tmp =  VAL_TO_LONG(accu) - 1;
			accu = LONG_TO_VAL(tmp);
			if( VAL_TO_LONG(accu) != tmp ) {
				goto raise_overflow;
			}
			break;
		case SMLADDINT:
			tmp = VAL_TO_LONG(*sp++) + VAL_TO_LONG(accu);
			accu = LONG_TO_VAL(tmp);
			if (VAL_TO_LONG(accu) != tmp) {
				goto raise_overflow;
			}
			break;
		case SMLSUBINT:
			tmp = VAL_TO_LONG(*sp++) - VAL_TO_LONG(accu);
			accu = LONG_TO_VAL(tmp);
			if (VAL_TO_LONG(accu) != tmp) {
				goto raise_overflow;
			}
			break;

#define ChunkLen (4 * sizeof(value) - 1)
#define MaxChunk ((1L << ChunkLen) - 1)

		case SMLMULINT:
		{
			long x, y;
			int isNegative = 0;
			x = VAL_TO_LONG(*sp++);
			y = VAL_TO_LONG(accu);
			if (x < 0) {
				x = -x; isNegative = 1;
			}
			if (y < 0) {
				y = -y; isNegative = !isNegative;
			}
			if (y > x) {
				tmp = y; y = x; x = tmp;
			}
			if (y > MaxChunk) {
				goto raise_overflow;
			}

			if( x <= MaxChunk ) {
				accu = LONG_TO_VAL(isNegative?(-(x * y)):(x * y));
			} else {
				tmp = (x >> ChunkLen) * y;
				if( tmp > MaxChunk + 1) {
					goto raise_overflow;
				}
				tmp = (tmp << ChunkLen) + (x & MaxChunk) * y;
				if( isNegative ) {
					tmp = - tmp;
				}

				accu = LONG_TO_VAL(tmp);
				if (VAL_TO_LONG(accu) != tmp) {
					goto raise_overflow;
				}
			}
		}
		break;

		case SMLDIVINT:
			tmp = VAL_TO_LONG(accu);
			accu = VAL_TO_LONG(*sp++);
			if (tmp == 0) {
				accu = Field(global_data, EXN_DIV);
				goto raise_exception;
			}
			if (tmp < 0) {
				accu = - accu;
				tmp = -tmp;
			}
			if( accu >= 0 ) {
				tmp = accu / tmp;
			} else {
				accu = - accu;
				if (accu % tmp == 0) {
					tmp = - (accu /tmp);
				} else {
					tmp = - (accu / tmp) - 1;
				}
			}

			accu = LONG_TO_VAL(tmp);
			if (VAL_TO_LONG(accu) != tmp) {
				goto raise_overflow;
			}
			break;

		case SMLMODINT:
		{
			long y;
			y = tmp = VAL_TO_LONG(accu);
			accu = VAL_TO_LONG(*sp++);
			if (tmp == 0)
			{
				accu = Field(global_data, EXN_DIV);
				goto raise_exception;
			}
			if (tmp < 0) {
				accu = -accu;
				tmp = -tmp;
			}

			if (accu >= 0) {
				tmp = accu % tmp;
			} else {
				accu = (-accu) % tmp;
				tmp = (accu == 0) ? 0 : (tmp - accu);
			}

			if( y < 0 ) {
				tmp = -tmp;
			}

			accu = LONG_TO_VAL(tmp);
			if( VAL_TO_LONG(accu) != tmp ) {
				goto raise_overflow;
			}
		}
		break;

		case MAKEREFVECTOR:
		{
			mlsize_t size = VAL_TO_LONG(sp[0]);
			sp[0] = accu;
			if (size == 0) {
				accu = Atom(Reference_tag);
			} else if (size < Max_young_wosize) {
				ALLOC_SMALL (accu, size, Reference_tag);
				do {size--; Field (accu, size) = *sp;} while (size != 0);
			} else if (IS_BLOCK (*sp) && Is_young (*sp)) {
				SETUP_FOR_GC;
				minor_collection ();
				tmp = alloc_shr (size, Reference_tag);
				RESTORE_AFTER_GC;
				accu = tmp;
				do {
					size--;
					Field (accu, size) = *sp;
				} while (size != 0);
			} else {
				SETUP_FOR_GC;
				tmp = alloc_shr (size, Reference_tag);
				RESTORE_AFTER_GC;
				accu = tmp;
				do {
					size--;
					initialize(&Field(accu, size), *sp);
				} while (size != 0);
			}
			sp++;
			break;
		}

		case SMLQUOTINT:
			tmp = accu - 1;
			if (tmp == 0) {
				accu = Field(global_data, EXN_DIV);
				goto raise_exception;
			}
			tmp = (*sp++ - 1) / tmp;
			accu = LONG_TO_VAL(tmp);
			if (VAL_TO_LONG(accu) != tmp) {
				goto raise_overflow;
			}
			break;
		case SMLREMINT:
			tmp = accu - 1;
			if (tmp == 0) {
				accu = Field(global_data, EXN_DIV);
				goto raise_exception;
			}

			accu = 1 + (*sp++ - 1) % tmp;
			break;

/* --- End of additional instructions for Moscow SML --- */

/* Machine control */

		case STOP:
			extern_sp = sp;
			external_raise = initial_external_raise;
			return accu;

		default:
			fatal_error("bad opcode");
			return Val_unit;		/* Can't reach the return */
		}
	}
}

extern value callback(value closure, value arg)
{
	value res;
	extern_sp -= 2;
	extern_sp[0] = arg;
	extern_sp[1] = closure;
	/* callback_depth++; */
	res = interprete(/* mode=exec */ 2, NULL, &callback1_code);
	/* callback_depth--; */
	return res;
}

extern value callback2(value closure, value arg1, value arg2)
{
	value res;
	extern_sp -= 3;
	extern_sp[0] = arg1;
	extern_sp[1] = arg2;
	extern_sp[2] = closure;
	/* callback_depth++; */
	res = interprete(/* mode=exec */ 2, NULL, &callback2_code);
	/* callback_depth--; */
	return res;
}

extern value callback3(value closure, value arg1, value arg2, value arg3)
{
	value res;
	extern_sp -= 4;
	extern_sp[0] = arg1;
	extern_sp[1] = arg2;
	extern_sp[2] = arg3;
	extern_sp[3] = closure;
	/* callback_depth++; */
	res = interprete(/* mode=exec */ 2, NULL, &callback3_code);
	/* callback_depth--; */
	return res;
}

/* end */


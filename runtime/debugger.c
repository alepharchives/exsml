#ifdef DEBUG

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "debugger.h"
#include "instruct.h"
#include "main.h"
#include "memory.h"
#include "mlvalues.h"
#include "prims.h"
#include "stacks.h"
#include "unalignd.h"

void print_pc(bytecode_t);

bytecode_t log_buffer[LOG_BUFFER_SIZE];
bytecode_t * log_ptr;
int trace_flag;

/* Displaying a heap object */

long max_print = 100;
long max_print_depth = 10;

long print_cnt;

static void print_val(value v, long d)
{
	long n;
	value * p;

	if (d <= 0) {
		printf(".");
		return;
	}

	print_cnt--;

	if (print_cnt <= 0) {
		if (print_cnt == 0) printf("...");
		return;
	}

	if (IS_LONG(v)) {
		printf("%ld", VAL_TO_LONG(v));
	} else if (!Is_in_heap (v) && !Is_young (v)) {
		printf("0x%lx", v);
	} else {
		switch(Tag_val(v)) {
		case String_tag:
			printf("\"%s\"", String_val(v));
			break;
		case Double_tag:
			printf("%g", Double_val(v));
			break;
		case Abstract_tag:
			printf("<abstract>");
			break;
		case Final_tag:
			printf("<finalized>");
			break;
		default:
			n = Tag_val(v);
			if (n < 26) {
				char c = (char) (n + 'A');
				printf ("%c", c);
			}else{
				printf("tag%ld", n);
			}
			n = Wosize_val(v);
			if (n > 0) {
				printf("(");
				p = &Field(v, 0);
				while (n > 1) {
					print_val(*p, d-1);
					printf(", ");
					p++;
					n--;
				}
				print_val(*p, d-1);
				printf(")");
			}
			break;
		}
	}
}

void print_value(value v)
{
	print_cnt = max_print;
	print_val(v, max_print_depth);
	printf("\n");
}



void print_pc(bytecode_t pc)
{
/* TODO: Push this back into the runtime config.h */
	printf(PC_FORMAT, pc - start_code);
}

/* Disassembling one instruction */

/*
 * We get the current instruction, the program counter, the accumulator, and
 * the stackpointer
 */
bytecode_t disasm_instr(int cur_instr, bytecode_t pc, value accu, value sp[])
{
	value accu_l;
	printf("Executing %s\n", names_of_instructions[cur_instr]);

	switch (cur_instr) {
	case BRANCHIFNOT:
		if (Tag_val(accu) == 0) {
			printf("True --> Jumping\n");
		} else {
			printf("False --> Continuing\n");
		}
		break;
	case EQ:
		printf("  Accu: ");
		print_value(accu);
		printf("  sp[0]: ");
		print_value(sp[0]);
		break;
	case GETGLOBAL:
		accu_l = Field(global_data, u32(pc));
		printf("  Global %i : ", u32(pc));
		print_value(accu_l);
		break;
	case GETFIELD0:
		printf("  Field0: ");
		print_value(Field(accu, 0));
		break;
	case GETFIELD1:
		printf("  Field1: ");
		print_value(Field(accu, 1));
		break;
	case GETFIELD2:
		printf("  Field2: ");
		print_value(Field(accu, 2));
		break;
	case GETFIELD3:
		printf("  Field3: ");
		print_value(Field(accu, 3));
		break;
	case GETFIELD:
		printf("  u16-pc: %i", u16(pc));
		printf("  Field: ");
		print_value(Field(accu, u16(pc)));
		break;
	case SETGLOBAL:
		printf("  Global %i : ", u32(pc));
		print_value(accu);
		break;
	case LTINT:
		printf("  sp[0] < accu\n");
		printf("  Accu: ");
		print_value(accu);
		printf("  sp[0]: ");
		print_value(sp[0]);
		printf("  Acc <-- ");
		print_value(Atom(sp[0] < accu));
		break;
	case MAKEBLOCK1:
		printf("  Tag: %i\n", (unsigned char)(*pc));
		printf("  Value: ");
		print_value(accu);
		break;
	case MAKEBLOCK2:
		printf("  Tag: %i\n", (unsigned char)(*pc));
		printf("  Field0: ");
		print_value(sp[0]);
		printf("  Field1: ");
		print_value(accu);
		break;
	case PUSH:
	case PUSHACC0:
		printf("  Pushed: ");
		print_value(accu);
		break;
	case PUSHACC1:
		printf("  Pushed: ");
		print_value(accu);
		printf("  Acc <-- ");
		print_value(sp[0]);
		break;
	case PUSHACC2:
		printf("  Pushed: ");
		print_value(accu);
		printf("  Acc <-- ");
		print_value(sp[1]);
		break;
	case PUSHACC3:
		printf("  Pushed: ");
		print_value(accu);
		printf("  Acc <-- ");
		print_value(sp[2]);
		break;
	case PUSHACC4:
		printf("  Pushed: ");
		print_value(accu);
		printf("  Acc <-- ");
		print_value(sp[3]);
		break;
	case PUSHACC5:
		printf("  Pushed: ");
		print_value(accu);
		printf("  Acc <-- ");
		print_value(sp[4]);
		break;
	case PUSHACC6:
		printf("  Pushed: ");
		print_value(accu);
		printf("  Acc <-- ");
		print_value(sp[5]);
		break;
	case PUSHACC7:
		printf("  Pushed: ");
		print_value(accu);
		printf("  Acc <-- ");
		print_value(sp[6]);
		break;
	case PUSHACC:
		printf("  u16-pc: %i", u16(pc));
		printf("  Pushed: ");
		print_value(accu);
		printf("  Acc <-- ");
		print_value(sp[u16(pc)-1]);
		break;
	case ACC0:
		printf("  Acc <-- ");
		print_value(sp[0]);
		break;
	case ACC1:
		printf("  Acc <-- ");
		print_value(sp[1]);
		break;
	case ACC2:
		printf("  Acc <-- ");
		print_value(sp[2]);
		break;
	case ACC3:
		printf("  Acc <-- ");
		print_value(sp[3]);
		break;
	case ACC4:
		printf("  Acc <-- ");
		print_value(sp[4]);
		break;
	case ACC5:
		printf("  Acc <-- ");
		print_value(sp[5]);
		break;
	case ACC6:
		printf("  Acc <-- ");
		print_value(sp[6]);
		break;
	case ACC7:
		printf("  Acc <-- ");
		print_value(sp[7]);
		break;
	case ACCESS:
		printf("  u16-pc: %i", u16(pc));
		printf("  Acc <-- ");
		print_value(sp[u16(pc)]);
		break;
	case C_CALL1:
		printf("  u16-pc: %i", u16(pc));
		printf("  Value: ");
		print_value(accu);
		printf("  C-fn: %i (%s)\n", u16(pc), names_of_cprim[u16(pc)]);
		break;
	case C_CALL2:
		printf("  u16-pc: %i", u16(pc));
		printf("  Value: ");
		print_value(accu);
		printf("  SP[1]: ");
		print_value(sp[1]);
		printf("  C-fn: %i (%s)\n", u16(pc), names_of_cprim[u16(pc)]);
	default:
		break;
	}

	return pc;
}

static unsigned long seed = 0x12345;

unsigned long not_random ()
{
	seed = seed * 65537 + 12345;
	return seed;
}

#endif /* DEBUG */

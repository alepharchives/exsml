#ifdef DEBUG

#include <stdio.h>
#include <stdlib.h>

#include "main.h"
#include "debugger.h"
#include "instruct.h"
#include "memory.h"
#include "mlvalues.h"
#include "opnames.h"
#include "stacks.h"
#include "unalignd.h"

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
				char c = (char) n + 'A';
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



void print_pc(pc)
     bytecode_t pc;
{
/* TODO: Push this back into the runtime config.h */
#ifdef SIXTYFOUR
	printf("%6ld  ", pc - start_code);
#else
	printf("%6d   ", pc - start_code);
#endif
}

/* Disassembling one instruction */

bytecode_t disasm_instr(bytecode_t pc)
{
	value accu;
	printf("Executing %s\n", names_of_instructions[*pc]);

	switch (*pc) {
	case GETGLOBAL:
		accu = Field(global_data, u16(pc));
		printf("Global %d : ", u16(pc));
		print_value(accu);
		break;
	default:
		break;
	}

	return pc;
}

void disasm(bytecode_t pc)
{
	int i;

	for (i = 0; i < 20; i++) {
		pc = disasm_instr(pc);
	}
}

void post_mortem(int n)
{
	bytecode_t * p;

	if (n > LOG_BUFFER_SIZE) n = LOG_BUFFER_SIZE;
	for (p = log_buffer +
		     (unsigned) (log_ptr - log_buffer - n) % LOG_BUFFER_SIZE;
	     n > 0;
	     n--) {
		disasm_instr(*p);
		p++;
		if (p >= log_buffer + LOG_BUFFER_SIZE) {
			p = log_buffer;
		}
	}
}

void failed_assert (char *expr, char *file, int line)
{
	fprintf (stderr, "Assertion failed: %s; file %s; line %d\n",
		 expr, file, line);
	exit (100);
}

static unsigned long seed = 0x12345;

unsigned long not_random ()
{
	seed = seed * 65537 + 12345;
	return seed;
}

#endif /* DEBUG */

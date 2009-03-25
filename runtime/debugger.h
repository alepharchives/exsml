#ifndef _debugger_
#define _debugger_

#include "misc.h"
#include "mlvalues.h"

#ifdef DEBUG

#define LOG_BUFFER_SIZE 100
extern bytecode_t log_buffer[LOG_BUFFER_SIZE];
extern bytecode_t * log_ptr;
extern int trace_flag;

#define Debug(x) x

void failed_assert (char *, char *, int);
void print_value (value);
bytecode_t disasm_instr (int, bytecode_t, value, value*);
void post_mortem (int);
unsigned long not_random (void);

#else /* DEBUG */

#define Debug(x)
#define Dprintx(x)

#endif /* DEBUG */

#define nTrace(msg, x, y)

#endif /* _debugger_ */

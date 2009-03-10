#ifndef _fail_
#define _fail_

#include <setjmp.h>
#include "mlvalues.h"

struct longjmp_buffer {
  jmp_buf buf;
};

extern struct longjmp_buffer * external_raise;
extern value exn_bucket;

extern void mlraise(value);
extern void raiseprimitive0(int exnindex);
extern void raiseprimitive1(int exnindex, value arg);
extern void raise_with_string(int exnindex, char * msg);
extern void failwith(char *);
extern void invalid_argument(char *);
extern void raise_overflow(void);
extern void raise_out_of_memory(void);
extern volatile int float_exn;

extern double maxdouble;

#endif /* _fail_ */

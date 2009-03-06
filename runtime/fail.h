#ifndef _fail_
#define _fail_

#include <setjmp.h>
#include "misc.h"
#include "mlvalues.h"

struct longjmp_buffer {
  jmp_buf buf;
};

extern struct longjmp_buffer * external_raise;
extern value exn_bucket;

extern Noreturn mlraise(value);
extern Noreturn raiseprimitive0(int exnindex);
extern Noreturn raiseprimitive1(int exnindex, value arg);
extern Noreturn raise_with_string(int exnindex, char * msg);
extern Noreturn failwith(char *);
extern Noreturn invalid_argument(char *);
extern Noreturn raise_overflow(void);
extern Noreturn raise_out_of_memory(void);
extern volatile int float_exn;

extern double maxdouble;

#endif /* _fail_ */

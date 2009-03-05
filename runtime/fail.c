/* Raising exceptions from C. */

#include <sys/param.h>
#include <float.h>

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"
#include "signals.h"
#include "globals.h"


/* float_exn is an exception index from globals.h.  The exception
   (Fail "floating point error") will be raised if float_exn has not
   been initialized before a floating point error occurs.  */

volatile int float_exn = SYS__EXN_FAIL;

double maxdouble = DBL_MAX/2;

struct longjmp_buffer * external_raise;
value exn_bucket;		/* ML type: string ref * 'a */

EXTERN void mlraise(value v)
{
  in_blocking_section = 0;
  exn_bucket = v;
  longjmp(external_raise->buf, 1);
}

/* Raise a unary pervasive exception with the given argument */

void raiseprimitive1(int exnindex, value arg) {
  value exn;
  Push_roots(r, 1);
  r[0] = arg;
  exn = alloc_tuple(2);
  modify(&Field(exn, 0), Field(global_data, exnindex));
  modify(&Field(exn, 1), r[0]);
  Pop_roots();
  mlraise(exn);
}

void raiseprimitive0(int exnindex) {
  raiseprimitive1(exnindex, Val_unit);
}

EXTERN void raise_with_string(int exnindex, char * msg) {
  raiseprimitive1(exnindex, copy_string(msg));
}

EXTERN void failwith (char* msg) {
  raise_with_string(SYS__EXN_FAIL, msg);
}

void invalid_argument (char * msg) {
  raise_with_string(SYS__EXN_ARGUMENT, msg);
}

void raise_out_of_memory() {
  raiseprimitive0(SYS__EXN_MEMORY);
}

void raise_overflow() {
  raiseprimitive0(SYS__EXN_OVERFLOW);
}

/* Allocation macros and functions */

#ifndef _memory_
#define _memory_

#include <assert.h>

#include "config.h"
#include "gc.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"

extern value *c_roots_head;

void init_c_roots (void);
value alloc_shr (mlsize_t, tag_t);
void adjust_gc_speed (mlsize_t, mlsize_t);
void modify (value *, value);
void initialize (value *, value);
char * stat_alloc (size_t);	         /* Size in bytes. */
void stat_free (char *);
char * stat_resize (char *, size_t);     /* Size in bytes. */


#define ALLOC_SMALL(result, wosize, tag) {				      \
  char *_res_ = young_ptr;						      \
  young_ptr += Bhsize_wosize (wosize);					      \
  if (young_ptr > young_end){						      \
    Setup_for_gc;							      \
    minor_collection ();						      \
    Restore_after_gc;							      \
    _res_ = young_ptr;							      \
    young_ptr += Bhsize_wosize (wosize);				      \
  }									      \
  Hd_hp (_res_) = Make_header ((wosize), (tag), Black);			      \
  (result) = Val_hp (_res_);						      \
}

/* [Push_roots] and [Pop_roots] are used for C variables that are GC roots.
 * It must contain all values in C local variables at the time the minor GC is
 * called.
 * Usage:
 * At the end of the declarations of your C local variables, add
 * [ Push_roots (variable_name, size); ]
 * The size is the number of declared roots.  They are accessed as
 * [ variable_name [0] ... variable_name [size - 1] ].
 * The [variable_name] and the [size] must not be [ _ ].
 * Just before the function return, add a call to [Pop_roots].
 */

#define PUSH_ROOTS(name, size)						      \
   value name [(size) + 2];						      \
   { long _; for (_ = 0; _ < (size); name [_++] = LONG_TO_VAL (0)); }	      \
   name [(size)] = (value) (size);					      \
   name [(size) + 1] = (value) c_roots_head;				      \
   c_roots_head = &(name [(size)]);

#define POP_ROOTS() {c_roots_head = (value *) c_roots_head [1]; }


#endif /* _memory_ */

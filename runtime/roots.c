/* To walk the memory roots for garbage collection */

#include "debugger.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"

void local_roots (void (*) (void));

void local_roots (void (*copy_fn) ())
{
	value *sp, *block;

	/* stack */
	for (sp = extern_sp; sp < stack_high; sp++) {
		copy_fn (sp, *sp);
	}

	/* C roots */
	for (block = c_roots_head; block != NULL; block = (value *) block [1]) {
		for (sp = block - (long) block [0]; sp < block; sp++) {
			copy_fn (sp, *sp);
		}
	}
}

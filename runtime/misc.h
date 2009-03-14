/* Miscellaneous macros and variables. */

#ifndef _misc_
#define _misc_

#include "runtime_config.h"
#include <stddef.h>

extern int verb_gc;
extern int volatile something_to_do;
extern int volatile force_minor_flag;

void force_minor_gc(void);
void gc_message(char *, unsigned long);
void fatal_error(char *);
void fatal_error_arg(char *, char *);
void memmov(char *, char *, unsigned long);
char * aligned_malloc(size_t, int);


#endif /* _misc_ */

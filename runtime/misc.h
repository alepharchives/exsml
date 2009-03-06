/* Miscellaneous macros and variables. */

#ifndef _misc_
#define _misc_

#include "runtime_config.h"
#include <stddef.h>

typedef size_t asize_t;

typedef char * addr;
#endif

#if defined(__STDC__) || defined(WIN32)
#define Volatile volatile
#else
#define Volatile
#endif

#define Noreturn void

extern int verb_gc;
extern int Volatile something_to_do;
extern int Volatile force_minor_flag;

void force_minor_gc(void);
void gc_message(char *, unsigned long);
Noreturn fatal_error(char *);
Noreturn fatal_error_arg(char *, char *);
void memmov(char *, char *, unsigned long);
char * aligned_malloc(asize_t, int);


#endif /* _misc_ */

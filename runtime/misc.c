#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "config.h"
#include "debugger.h"
#include "misc.h"
#include "io.h"
#include "sys.h"

int verb_gc;
int volatile something_to_do = 0;
int volatile force_minor_flag = 0;

void force_minor_gc (void)
{
  force_minor_flag = 1;
  something_to_do = 1;
}

void gc_message (char * msg, unsigned long arg)
{
  if (verb_gc){
    fprintf (stderr, msg, arg);
    fflush (stderr);
  }
}

void fatal_error (char * msg)
{
  flush_stdouterr();
  fprintf (stderr, "%s", msg);
  sys_exit(Val_int(2));
}

void fatal_error_arg (char * fmt, char * arg)
{
  flush_stdouterr();
  fprintf (stderr, fmt, arg);
  sys_exit(Val_int(2));
}

char *aligned_malloc (size_t size, int modulo)
{
  char *raw_mem;
  unsigned long aligned_mem;

  assert (modulo < Page_size);
  raw_mem = malloc (size + Page_size);
  if (raw_mem == NULL) return NULL;
  raw_mem += modulo;		/* Address to be aligned */
  aligned_mem = (((unsigned long) raw_mem / Page_size + 1) * Page_size);
  return (char *) (aligned_mem - modulo);
}

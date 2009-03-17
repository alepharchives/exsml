/* Primitives for the toplevel */

#include <stdlib.h>
#include <string.h>
#include "config.h"
#include "alloc.h"
#include "global_tbl.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "mlvalues.h"
#include "prims.h"

extern value interprete(int mode, bytecode_t bprog,
			int code_size, bytecode_t* rprog);

value start_interp(value may_free, value prog, value offset, value vlen) /* ML */
{
  bytecode_t bprog = (bytecode_t)&Byte(prog, VAL_TO_LONG(offset)); // In ML heap
  int len = VAL_TO_LONG(vlen);
  value res;

#if defined(WORDS_BIGENDIAN) && !defined(HAVE_ALIGNED_ACCESS_REQUIRED)
  fixup_endianness(&Byte(prog, 0), (size_t) len);
#endif

  {
    // Copy bytecode to memory outside the ML heap
    bytecode_t actualprog = (bytecode_t)malloc(len);
    memmove(actualprog, bprog, len);
    res = interprete(/* mode=byte exec */ 1, actualprog, len, NULL);
    if (Bool_val(may_free))
      free(actualprog);		// Allocated above
  }

  return res;
}

value realloc_global(size)      /* ML */
     value size;
{
  mlsize_t requested_size, actual_size, i;
  value new_global_data;

  requested_size = VAL_TO_LONG(size);
  actual_size = Wosize_val(global_data);
  if (requested_size >= actual_size) {
    requested_size = (requested_size + 0x100) & 0xFFFFFF00;
    new_global_data = alloc_shr(requested_size, 0);
    for (i = 0; i < actual_size; i++)
      initialize(&Field(new_global_data, i), Field(global_data, i));
    for (i = actual_size; i < requested_size; i++){
      Field (new_global_data, i) = LONG_TO_VAL (0);
    }
    modify(&Field(new_global_data, GLOBAL_DATA), new_global_data);
    global_data = new_global_data;
  }
  return Atom(0);
}


value static_alloc(size)        /* ML */
     value size;
{
  return (value) stat_alloc((size_t) VAL_TO_LONG(size));
}

value static_free(blk)          /* ML */
     value blk;
{
  stat_free((char *) blk);
  return Atom(0);
}

value static_resize(blk, new_size) /* ML */
     value blk, new_size;
{
  return (value) stat_resize((char *) blk, (size_t) VAL_TO_LONG(new_size));
}

value obj_is_block(arg)             /* ML */
     value arg;
{
  return Atom(IS_BLOCK(arg));
}

value obj_block(tag, size) /* ML */
     value tag, size;
{
  value res;
  mlsize_t sz, i;
  tag_t tg;

  sz = VAL_TO_LONG(size);
  tg = VAL_TO_LONG(tag);
  if (sz == 0) return Atom(tg);
  res = alloc(sz, tg);
  for (i = 0; i < sz; i++)
    Field(res, i) = LONG_TO_VAL(0);

  return res;
}

value available_primitives()    /* ML */
{
  return copy_string_array(names_of_cprim);
}

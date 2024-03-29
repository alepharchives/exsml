#ifndef _minor_gc_
#define _minor_gc_


#include "misc.h"

extern char *young_start, *young_ptr, *young_end;
extern value **ref_table_ptr, **ref_table_limit;
extern size_t minor_heap_size;

#define Is_young(val) \
  ((char *)(val) > (char *)young_start && (char *)(val) < (char *)young_end)

extern void set_minor_heap_size (size_t);
extern void minor_collection (void);
extern void realloc_ref_table (void);


#endif /* _minor_gc_ */

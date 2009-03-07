#ifndef _gc_ctrl_
#define _gc_ctrl_

#include "misc.h"

extern size_t stat_minor_words;
extern size_t stat_promoted_words;
extern size_t stat_major_words;
extern size_t stat_minor_collections;
extern size_t stat_major_collections;
extern size_t stat_heap_size;

void init_gc (long, long, int, int);


#endif /* _gc_ctrl_ */

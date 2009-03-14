#ifndef _major_gc_
#define _major_gc_


#include "freelist.h"
#include "misc.h"

typedef struct {
  size_t size;
  char *next;
} heap_chunk_head;

extern int gc_phase;
extern unsigned long allocated_words;
extern unsigned long extra_heap_memory;

#define Phase_mark 0
#define Phase_weak 1
#define Phase_sweep 2

extern char *heap_start;
extern char *heap_end;
extern unsigned long total_heap_size;
extern char *page_table;
extern size_t page_table_size;
extern char *gc_sweep_hp;

#define In_heap 1
#define Not_in_heap 0

#define Page(p) (((char *) (p) - (char *) heap_start) >> Page_log)
#define Is_in_heap(p) \
  ((char *)(p) >= (char *)heap_start && (char *)(p) < (char *)heap_end \
   && page_table [Page (p)] == In_heap)

void init_major_heap (size_t);
size_t round_heap_chunk_size (size_t);
void darken (value);
void major_collection_slice (void);
void major_collection (void);
void finish_major_cycle (void);

#endif /* _major_gc_ */

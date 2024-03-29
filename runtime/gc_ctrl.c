/* mosml/src/runtime/gc_ctrl.c
   Updated 2008-03-05 to prevent malloc from using mmap()
*/
#include <malloc.h>
#include <stdio.h>
#include <assert.h>

#include "alloc.h"
#include "debugger.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "mlvalues.h"
#include "memory.h"

size_t stat_minor_words = 0;
size_t stat_promoted_words = 0;
size_t stat_major_words = 0;
size_t stat_minor_collections = 0;
size_t stat_major_collections = 0;
size_t stat_heap_size = 0;           /* bytes */

extern size_t major_heap_increment;  /* bytes; cf. major_gc.c */
extern int percent_free;              /*        cf. major_gc.c */
extern int verb_gc;                   /*        cf. misc.c */

#define Chunk_size(c) (((heap_chunk_head *) (c)) [-1]).size
#define Chunk_next(c) (((heap_chunk_head *) (c)) [-1]).next
#define Next(hp) ((hp) + Bhsize_hp (hp))

value gc_stat(value);
value gc_get(value);
value gc_set(value);
value gc_minor(value);
value gc_major(value);
value gc_full_major(value);

/* This will also thoroughly verify the heap if compiled in DEBUG mode. */

value gc_stat (value v)
{
  value res;
  long live_words = 0, live_blocks = 0,
       free_words = 0, free_blocks = 0, largest_free = 0,
       fragments = 0, heap_chunks = 0;
  char *chunk = heap_start, *chunk_end;
  char *cur_hp, *prev_hp;
  header_t cur_hd;

  assert (v == Atom (0));

  while (chunk != NULL){
    ++ heap_chunks;
    chunk_end = chunk + Chunk_size (chunk);
    prev_hp = NULL;
    cur_hp = chunk;
    while (cur_hp < chunk_end){
      cur_hd = Hd_hp (cur_hp);
      switch (Color_hd (cur_hd)){
      case White:
	if (Wosize_hd (cur_hd) == 0){
	  ++fragments;
	  assert (prev_hp == NULL
		  || (Color_hp (prev_hp) != Blue
		      && Wosize_hp (prev_hp) > 0));
	  assert (Next (cur_hp) == chunk_end
		  || (Color_hp (Next (cur_hp)) != Blue
		      && Wosize_hp (Next (cur_hp)) > 0));
	  break;
	}
	/* FALLTHROUGH */
      case Gray: case Black:
	assert (Wosize_hd (cur_hd) > 0);
	++ live_blocks;
	live_words += Whsize_hd (cur_hd);
	break;
      case Blue:
	assert (Wosize_hd (cur_hd) > 0);
	++ free_blocks;
	free_words += Whsize_hd (cur_hd);
	if (Whsize_hd (cur_hd) > largest_free){
	  largest_free = Whsize_hd (cur_hd);
	}
	assert (prev_hp == NULL
		|| (Color_hp (prev_hp) != Blue
		    && Wosize_hp (prev_hp) > 0));
	assert (Next (cur_hp) == chunk_end
		|| (Color_hp (Next (cur_hp)) != Blue
		    && Wosize_hp (Next (cur_hp)) > 0));
	break;
      default:
	      perror("Impossible");
      }
      prev_hp = cur_hp;
      cur_hp = Next (cur_hp);
    }                                          assert (cur_hp == chunk_end);
    chunk = Chunk_next (chunk);
  }

  assert (live_words + free_words + fragments == Wsize_bsize (stat_heap_size));
  /* Order of elements changed for Moscow ML */
  res = alloc (13, 0);
  Field (res, 11) = LONG_TO_VAL (stat_minor_words
                             + Wsize_bsize (young_ptr - young_start));
  Field (res, 12) = LONG_TO_VAL(stat_promoted_words);
  Field (res,  9) = LONG_TO_VAL (stat_major_words + allocated_words);
  Field (res, 10) = LONG_TO_VAL(stat_minor_collections);
  Field (res,  8) = LONG_TO_VAL(stat_major_collections);
  Field (res,  4) = LONG_TO_VAL (Wsize_bsize (stat_heap_size));
  Field (res,  3) = LONG_TO_VAL(heap_chunks);
  Field (res,  7) = LONG_TO_VAL(live_words);
  Field (res,  6) = LONG_TO_VAL(live_blocks);
  Field (res,  2) = LONG_TO_VAL(free_words);
  Field (res,  1) = LONG_TO_VAL(free_blocks);
  Field (res,  5) = LONG_TO_VAL(largest_free);
  Field (res,  0) = LONG_TO_VAL(fragments);
  return res;
}

value gc_get (value v)
{
  value res;

  assert (v == Atom (0));
  /* Order of elements changed for Moscow ML */
  res = alloc (4, 0);
  Field (res, 1) = Wsize_bsize (LONG_TO_VAL(minor_heap_size));
  Field (res, 0) = Wsize_bsize (LONG_TO_VAL(major_heap_increment));
  Field (res, 2) = LONG_TO_VAL(percent_free);
  Field (res, 3) = Val_bool (verb_gc);
  return res;
}

static int norm_pfree (int p)
{
	return (p < 1) ? 1 : p;
}

static long norm_heapincr (long i)
{
  i = ((i + (1 << Page_log) - 1) >> Page_log) << Page_log;
  if (i < Heap_chunk_min) i = Heap_chunk_min;
  if (i > Heap_chunk_max) i = Heap_chunk_max;
  return i;
}

static long norm_minsize (long s)
{
  if (s < Minor_heap_min) s = Minor_heap_min;
  if (s > Minor_heap_max) s = Minor_heap_max;
  return s;
}

value gc_set (value v)
{
  int newpf;
  /* Order of elements changed for Moscow ML */
  verb_gc = Bool_val (Field (v, 3));

  newpf = norm_pfree (VAL_TO_LONG (Field (v, 2)));
  if (newpf != percent_free){
    percent_free = newpf;
    gc_message ("New space overhead: %d%%\n", percent_free);
  }

  if (Bsize_wsize (VAL_TO_LONG (Field (v, 0))) != major_heap_increment){
    major_heap_increment = norm_heapincr (Bsize_wsize (VAL_TO_LONG (Field(v,0))));
    gc_message ("New heap increment size: %ldk\n", major_heap_increment/1024);
  }

    /* Minor heap size comes last because it will trigger a minor collection
       (thus invalidating [v]) and it can raise [Out_of_memory]. */
  if (Bsize_wsize (VAL_TO_LONG (Field (v, 1))) != minor_heap_size){
    long new_size = norm_minsize (Bsize_wsize (VAL_TO_LONG (Field (v, 1))));
    gc_message ("New minor heap size: %ldk\n", new_size/1024);
    set_minor_heap_size (new_size);
  }
  return Atom (0);
}

value gc_minor (value v)
{
	assert (v == Atom (0));
	minor_collection ();
	return Atom (0);
}

value gc_major (value v)
{
	assert (v == Atom (0));
	minor_collection ();
	finish_major_cycle ();
	return Atom (0);
}

value gc_full_major (value v)
{
	assert (v == Atom (0));
	minor_collection ();
	finish_major_cycle ();
	finish_major_cycle ();
	return Atom (0);
}

void init_gc (long minor_size, long major_incr, int percent_fr, int verb)
{
#ifdef DEBUG
	gc_message ("*** exsmlrunm: debug mode ***\n", 0);
#endif
	verb_gc = verb;
	/* Added 2008-03-05 to prevent malloc from using mmap() */
	mallopt(M_MMAP_MAX, 0);
	set_minor_heap_size (Bsize_wsize (norm_minsize (minor_size)));
	major_heap_increment = Bsize_wsize (norm_heapincr (major_incr));
	percent_free = norm_pfree (percent_fr);
	init_major_heap (major_heap_increment);
	init_c_roots ();
	gc_message ("Initial space overhead: %d%%\n", percent_free);
	gc_message ("Initial heap increment: %ldk\n", major_heap_increment / 1024);
	gc_message ("Initial minor heap size: %ldk\n", minor_heap_size / 1024);
}

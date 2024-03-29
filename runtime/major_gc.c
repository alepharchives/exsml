#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "attributes.h"
#include "config.h"
#include "debugger.h"
#include "fail.h"
#include "freelist.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "global_tbl.h"
#include "major_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"

#include "runtime.h"

#include <limits.h>

int percent_free;
size_t major_heap_increment;
char *heap_start, *heap_end;
char *page_table;
size_t page_table_size;
char *gc_sweep_hp;
int gc_phase;

/* The mark phase will register pointers to live arrays of weak
   pointers in weak_arrays.  Then the weak phase traverses each weak
   array and resets pointers to objects that will be deallocated by the
   sweep phase:
*/

static value *weak_arrays;
value *weak_arrays_cur, *weak_arrays_end;
static size_t weak_arrays_size;

static value *gray_vals;
value *gray_vals_cur, *gray_vals_end;
static size_t gray_vals_size;
static int heap_is_pure;   /* The heap is pure if the only gray objects
                              below [markhp] are also in [gray_vals]. */
unsigned long allocated_words;
unsigned long extra_heap_memory;
extern char *fl_merge;  /* Defined in freelist.c. */

static char *markhp, *chunk, *limit;

static void realloc_gray_vals (void)
{
  value *new;

  assert (gray_vals_cur == gray_vals_end);
  if (gray_vals_size < stat_heap_size / 128){
    gc_message ("Growing gray_vals to %ldk\n",
		(long) gray_vals_size * sizeof (value) / 512);
    new = (value *) realloc ((char *) gray_vals,
                             2 * gray_vals_size * sizeof (value));
    if (new == NULL){
      gc_message ("No room for growing gray_vals\n", 0);
      gray_vals_cur = gray_vals;
      heap_is_pure = 0;
    }else{
      gray_vals = new;
      gray_vals_cur = gray_vals + gray_vals_size;
      gray_vals_size *= 2;
      gray_vals_end = gray_vals + gray_vals_size;
    }
  }else{
    gray_vals_cur = gray_vals + gray_vals_size / 2;
    heap_is_pure = 0;
  }
}

static void realloc_weak_arrays (void)
{
  value *new;

  assert (weak_arrays_cur == weak_arrays_end);
  gc_message ("Growing weak_arrays to %ld\n",
              (long) weak_arrays_size * 2);
  new = (value *) realloc ((char *) weak_arrays,
                             2 * weak_arrays_size * sizeof (value));
  if (new == NULL){
    fatal_error ("Fatal error: cannot grow weak_arrays table.\n");
  }else{
    weak_arrays = new;
    weak_arrays_cur = weak_arrays + weak_arrays_size;
    weak_arrays_size *= 2;
    weak_arrays_end = weak_arrays + weak_arrays_size;
  }
}

void darken (value v)
{
  if (IS_BLOCK(v) && Is_in_heap (v) && Is_white_val (v)){
    Hd_val (v) = Grayhd_hd (Hd_val (v));
    *gray_vals_cur++ = v;
    if (gray_vals_cur >= gray_vals_end) realloc_gray_vals ();
  }
}

static void darken_root (value *UNUSED(p), value v)
{
	darken (v);
}

static void start_cycle (void)
{
  assert (gray_vals_cur == gray_vals);
  assert (Is_white_val (global_data));
  darken (global_data);
  local_roots (darken_root);
  gc_phase = Phase_mark;
  markhp = NULL;
}

static void mark_slice (long work)
{
  value v, child;
  mlsize_t i;

  while (work > 0){
    if (gray_vals_cur > gray_vals){
      v = *--gray_vals_cur;
      assert (Is_gray_val (v));
      Hd_val (v) = Blackhd_hd (Hd_val (v));
      if (Tag_val (v) < No_scan_tag){
	for (i = Wosize_val (v); i > 0;){
	  --i;
	  child = Field (v, i);
	  darken (child);
	}
      } else if (Tag_val(v) == Weak_tag) {
        *weak_arrays_cur++ = v;
        if (weak_arrays_cur >= weak_arrays_end) realloc_weak_arrays ();
      }
      work -= Whsize_val (v);
    }else if (markhp != NULL){
      if (markhp == limit){
	chunk = (((heap_chunk_head *) chunk) [-1]).next;
	if (chunk == NULL){
	  markhp = NULL;
	}else{
	  markhp = chunk;
	  limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
	}
      }else{
	if (Is_gray_val (Val_hp (markhp))){
	  assert (gray_vals_cur == gray_vals);
	  *gray_vals_cur++ = Val_hp (markhp);
	}
	markhp += Bhsize_hp (markhp);
      }
    }else if (!heap_is_pure){
      heap_is_pure = 1;
      chunk = heap_start;
      markhp = chunk;
      limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
    }else{
      /* Marking is done. */
      gc_sweep_hp = heap_start;
      fl_init_merge ();
      gc_phase = Phase_weak;
      chunk = heap_start;
      gc_sweep_hp = chunk;
      limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
      work = 0;
    }
  }
}

/* Reset weak pointers to objects that will be deallocated by the sweep phase
 */

static void weak_phase()
{
  value *c;
  for (c = weak_arrays; c < weak_arrays_cur; c++)
    {
      int i;
      value arr = *c;
      int len = Wosize_val(arr);
      for (i=0; i < len; i++)
	{
	  value v = Field(arr, i);
	  if (IS_BLOCK(v) && Is_in_heap(v) && Is_white_val(v))
	    Field(arr, i) = (value)NULL;
	}
    }
  weak_arrays_cur = weak_arrays;
  gc_phase = Phase_sweep;
}

static void sweep_slice (long work)
{
	char *hp;
	header_t hd;

	while (work > 0) {
		if (gc_sweep_hp < limit) {
			hp = gc_sweep_hp;
			hd = Hd_hp (hp);
			work -= Whsize_hd (hd);
			gc_sweep_hp += Bhsize_hd (hd);
			switch (Color_hd (hd)) {
			case White:
				if (Tag_hd (hd) == Final_tag) {
					Final_fun (Val_hp (hp)) (Val_hp (hp));
				}
				gc_sweep_hp = fl_merge_block (Bp_hp (hp));
				break;
			case Gray:
				assert (0);     /* Fall through to Black when not in debug mode. */
			case Black:
				Hd_hp (hp) = Whitehd_hd (hd);
				break;
			case Blue:
				/* Only the blocks of the free-list are blue.  See [freelist.c]. */
				fl_merge = Bp_hp (hp);
				break;
			default:
				perror("Unknown GC Color!");
			}
			assert (gc_sweep_hp <= limit);
		} else {
			chunk = (((heap_chunk_head *) chunk) [-1]).next;
			if (chunk == NULL){
				/* Sweeping is done.  Start the next cycle. */
				++ stat_major_collections;
				work = 0;
				start_cycle ();
			}else{
				gc_sweep_hp = chunk;
				limit = chunk + (((heap_chunk_head *) chunk) [-1]).size;
			}
		}
	}
}

void major_collection_slice (void)
{
  /* Free memory at the start of the GC cycle:
                 FM = stat_heap_size * percent_free / 100 * 2/3
     Proportion of free memory consumed since the previous slice:
                 PH = allocated_words / FM
     Proportion of extra-heap memory consumed since the previous slice:
                 PE = extra_heap_memory / stat_heap_size
     Proportion of total work to do in this slice:
                 P  = PH + PE
     Amount of marking work for the GC cycle:
                 MW = stat_heap_size * (100 - percent_free) / 100
     Amount of sweeping work for the GC cycle:
                 SW = stat_heap_size
     Amount of marking work for this slice:
                 MS = MW * 2 * P
                 MS = 2 * (100 - percent_free)
                      * (allocated_words * 3 / percent_free / 2
		         + 100 * extra_heap_memory)
     Amount of sweeping work for this slice:
                 SS = SW * 2 * P
                 SS = 2 * 100
		      * (allocated_words * 3 / percent_free / 2
		         + 100 * extra_heap_memory)
     This slice will either mark MS words or sweep SS words.
  */

#define Margin 100  /* Make it a little faster to be on the safe side. */

  if (gc_phase == Phase_mark){
    mark_slice (2 * (100 - percent_free)
		* (allocated_words * 3 / percent_free / 2
                   + 100 * extra_heap_memory)
		+ Margin);
    gc_message ("!", 0);
  }else if (gc_phase == Phase_weak){
    weak_phase();
    gc_message (".", 0);
  }else{
    assert (gc_phase == Phase_sweep);
    sweep_slice (200 * (allocated_words * 3 / percent_free / 2
			+ 100 * extra_heap_memory)
		 + Margin);
    gc_message ("$", 0);
  }
  stat_major_words += allocated_words;
  allocated_words = 0;
  extra_heap_memory = 0;
}

/* The minor heap must be empty when this function is called. */
void finish_major_cycle (void)
{

  beg_gc_time();

  if (gc_phase == Phase_mark) mark_slice (LONG_MAX);
  if (gc_phase == Phase_weak) weak_phase();
  assert (gc_phase == Phase_sweep);
  sweep_slice (LONG_MAX);
  stat_major_words += allocated_words;
  allocated_words = 0;

  end_gc_time();

}

size_t round_heap_chunk_size (size_t request)
{
	assert (major_heap_increment >= Heap_chunk_min);
	if (request < major_heap_increment) {
		assert (major_heap_increment % Page_size == 0);
		return major_heap_increment;
	} else if (request <= Heap_chunk_max) {
		return ((request + Page_size - 1) >> Page_log) << Page_log;
	} else {
		raise_out_of_memory ();
	}
	return 0;			/* Can't reach return */
}

void init_major_heap (size_t heap_size)
{
  size_t i;

  stat_heap_size = round_heap_chunk_size (heap_size);
  assert (stat_heap_size % Page_size == 0);
  heap_start = aligned_malloc (stat_heap_size + sizeof (heap_chunk_head),
			       sizeof (heap_chunk_head));
  if (heap_start == NULL)
    fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  heap_start += sizeof (heap_chunk_head);
  assert ((unsigned long) heap_start % Page_size == 0);
  (((heap_chunk_head *) heap_start) [-1]).size = stat_heap_size;
  (((heap_chunk_head *) heap_start) [-1]).next = NULL;
  heap_end = heap_start + stat_heap_size;
  assert ((unsigned long) heap_end % Page_size == 0);
  page_table_size = 4 * stat_heap_size / Page_size;
  page_table = (char *) malloc (page_table_size);
  if (page_table == NULL){
    fatal_error ("Fatal error: not enough memory for the initial heap.\n");
  }
  for (i = 0; i < page_table_size; i++){
    page_table [i] = Not_in_heap;
  }
  for (i = Page (heap_start); i < Page (heap_end); i++){
    page_table [i] = In_heap;
  }
  Hd_hp (heap_start) = Make_header (Wosize_bhsize (stat_heap_size), 0, Blue);
  fl_init_merge ();
  fl_merge_block (Bp_hp (heap_start));
  /* We start the major GC in the marking phase, just after the roots have been
     darkened. (Since there are no roots, we don't have to darken anything.) */
  gc_phase = Phase_mark;
  weak_arrays_size = 1;
  weak_arrays = (value *) malloc (weak_arrays_size * sizeof (value));
  weak_arrays_cur = weak_arrays;
  weak_arrays_end = weak_arrays + weak_arrays_size;
  gray_vals_size = 2048;
  gray_vals = (value *) malloc (gray_vals_size * sizeof (value));
  gray_vals_cur = gray_vals;
  gray_vals_end = gray_vals + gray_vals_size;
  heap_is_pure = 1;
  allocated_words = 0;
  extra_heap_memory = 0;
}

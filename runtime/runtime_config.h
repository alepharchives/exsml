#ifndef _config_
#define _config_

#include <limits.h>

#include "mosml_config.h"

/* Determine if a char is unsigned or not */

#if CHAR_MIN == 0
 #define CHAR_UNSIGNED 1
#endif

/* Signed char type */

#if defined(__STDC__) || !defined(CHAR_UNSIGNED)
typedef signed char schar;
#else
typedef char schar;
#endif

/* Do not change this definition. */
#define Page_size (1 << Page_log)

/* Memory model parameters */

/* The size of a page for memory management (in bytes) is [1 << Page_log].
   It must be a multiple of [sizeof (long)]. */
#define Page_log 12             /* A page is 4 kilobytes. */

/* Initial sizes of stacks (bytes). */
#define Stack_size 32768

/* Minimum free size of stacks (bytes); below that, they are reallocated. */
#define Stack_threshold 2048

/* Maximum sizes for the stacks (bytes). */

#define Max_stack_size 1048576

/* Maximum size of a block allocated in the young generation (words). */
/* Must be > 4 */
#define Max_young_wosize 256


/* Minimum size of the minor zone (words).
   This must be at least [Max_young_wosize + 1]. */
#define Minor_heap_min 4096

/* Maximum size of the minor zone (words).
   Must be greater than or equal to [Minor_heap_min].
*/
#define Minor_heap_max (1 << 28)

/* Default size of the minor zone. (words)  */
#define Minor_heap_def 32768


/* Minimum size increment when growing the heap (words).
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_min (2 * Page_size / sizeof (value))

/* Maximum size of a contiguous piece of the heap (words).
   Must be greater than or equal to [Heap_chunk_min].
   Must be greater than or equal to [Bhsize_wosize (Max_wosize)]. */
#define Heap_chunk_max (Bhsize_wosize (Max_wosize))

/* Default size increment when growing the heap. (bytes)
   Must be a multiple of [Page_size / sizeof (value)]. */
#define Heap_chunk_def (62 * Page_size / sizeof (value))


/* Default speed setting for the major GC.  The heap will grow until
   the dead objects and the free list represent this percentage of the
   heap size.  The rest of the heap is live objects. */
#define Percent_free_def 30

#endif /* _config_ */

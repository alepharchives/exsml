/* The generic hashing primitive */

#include "mlvalues.h"
#include "memory.h"
#include "str.h"

static unsigned long hash_accu;
static long hash_univ_limit, hash_univ_count;
static char safe;

value hash_univ_param(value, value, value);
value hash_univ_safe_param(value, value, value);
static void hash_aux(value);

value hash_univ_param(value count, value limit, value obj)
{
	hash_univ_limit = VAL_TO_LONG(limit);
	hash_univ_count = VAL_TO_LONG(count);
	hash_accu = 0;
	safe = 0;
	hash_aux(obj);
	return LONG_TO_VAL(hash_accu & 0x3FFFFFFF);
	/* The & has two purposes: ensure that the return value is positive
	   and give the same result on 32 bit and 64 bit architectures. */
}

value hash_univ_safe_param(value count, value limit, value obj)
{
	hash_univ_limit = VAL_TO_LONG(limit);
	hash_univ_count = VAL_TO_LONG(count);
	hash_accu = 0;
	safe = 1;
	hash_aux(obj);
	return LONG_TO_VAL(hash_accu & 0x3FFFFFFF);
	/* The & has two purposes: ensure that the return value is positive
	   and give the same result on 32 bit and 64 bit architectures. */
}

#define Alpha 65599
#define Beta 19
#define Combine(new)  (hash_accu = hash_accu * Alpha + (new))
#define Combine_small(new) (hash_accu = hash_accu * Beta + (new))

static void hash_aux(value obj)
{
  unsigned char * p;
  mlsize_t i;
  tag_t tag;

  hash_univ_limit--;
  if (hash_univ_count < 0 || hash_univ_limit < 0) {
	  if (safe) {
		  fatal_error("hash: count limit exceeded\n");
	  } else {
		  return;
	  }
  }

  if (IS_LONG(obj)) {
	  hash_univ_count--;
	  Combine(VAL_TO_LONG(obj));
	  return;
  }

  /* Atoms are not in the heap, but it's better to hash their tag
     than to do nothing. */

  if (Is_atom(obj)) {
    tag = Tag_val(obj);
    hash_univ_count--;
    Combine_small(tag);
    return;
  }

  /* Pointers into the heap are well-structured blocks.
     We can inspect the block contents. */

  if (Is_in_heap(obj) || Is_young(obj)) {
    tag = Tag_val(obj);
    switch (tag) {
    case String_tag:
      hash_univ_count--;
      {
	      mlsize_t len = string_length(obj);
	      i = len <= 128 ? len : 128;
	      // Hash on 128 first characters
	      for (p = &Byte_u(obj, 0); i > 0; i--, p++) {
		      Combine_small(*p);
	      }
	      // Hash on logarithmically many additional characters beyond 128
	      for (i = 1; i+127 < len; i *= 2) {
		      Combine_small(Byte_u(obj, 127+i));
	      }
	      break;
      }
    case Double_tag:
      /* For doubles, we inspect their binary representation, LSB first.
         The results are consistent among all platforms with IEEE floats. */
      hash_univ_count--;
#ifdef WORDS_BIGENDIAN
      for (p = &Byte_u(obj, sizeof(double) - 1), i = sizeof(double);
           i > 0;
           p--, i--)
#else
      for (p = &Byte_u(obj, 0), i = sizeof(double);
           i > 0;
           p++, i--)
#endif
        Combine_small(*p);
      break;
    case Abstract_tag:
    case Final_tag:
      /* We don't know anything about the contents of the block.
         Better do nothing. */
      break;
    case Reference_tag:
      /* We can't hash on the heap address itself, since the reference block
       * may be moved (from the young generation to the old one).
       * But, we may follow the pointer.  On cyclic structures this will
       * terminate because the hash_univ_count gets decremented.
       */

      /* Poor idea to hash on the pointed-to structure, even so: it may change,
       * and hence the hash value of the value changes, although the ref
       * doesn't.
       *
       * This breaks most hash table implementations.  sestoft 2000-02-20.
       */

	    if (safe) {
		    fatal_error("hash: ref encountered\n");
	    }
	    Combine_small(tag);
	    hash_univ_count--;
      break;
    default:
      hash_univ_count--;
      Combine_small(tag);
      i = Wosize_val(obj);
      while (i != 0) {
        i--;
        hash_aux(Field(obj, i));
      }
      break;
    }
    return;
  }

  /* Otherwise, obj is a pointer outside the heap, to an object with
     a priori unknown structure. Use its physical address as hash key. */
  Combine((long) obj);
}

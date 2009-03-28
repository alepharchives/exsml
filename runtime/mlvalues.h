#ifndef _mlvalues_
#define _mlvalues_


#include "mosml_config.h"
#include "misc.h"

/* Definitions

  word: Four bytes on 32 and 16 bit architectures,
        eight bytes on 64 bit architectures.
  long: A C long integer.
  val: The ML representation of something.  A long or a block or a pointer
       outside the heap.  If it is a block, it is the (encoded) address
       of an object.  If it is a long, it is encoded as well.
  object: Something allocated.  It always has a header and some
          fields or some number of bytes (a multiple of the word size).
  field: A word-sized val which is part of an object.
  bp: Pointer to the first byte of an object.  (a char *)
  op: Pointer to the first field of an object.  (a value *)
  hp: Pointer to the header of an object.  (a char *)
  int32_t: Four bytes on all architectures.

  Remark: An object size is always a multiple of the word size, and at least
          one word plus the header.

  bosize: Size (in bytes) of the "bytes" part.
  wosize: Size (in words) of the "fields" part.
  bhsize: Size (in bytes) of the object with its header.
  whsize: Size (in words) of the object with its header.

  hd: A header.
  tag: The value of the tag field of the header.
  color: The value of the color field of the header.
         This is for use only by the GC.
*/

typedef long value;
typedef unsigned long header_t;
typedef unsigned long mlsize_t;
typedef unsigned int tag_t;             /* Actually, an unsigned char */
typedef unsigned long color_t;
typedef unsigned long mark_t;

/* Longs vs blocks. */
#define IS_LONG(x)   (((x) & 1) == 1)
#define IS_BLOCK(x)  (((x) & 1) == 0)

/* Conversion macro names are always of the form  "to_from". */
/* Example: Val_long as in "Val from long" or "Val of long". */
#define LONG_TO_VAL(x)     (((long)(x) << 1) + 1)
#define VAL_TO_LONG(x)     ((x) >> 1)
#define MAX_TAGGED_LONG ((long)((1L << (8 * sizeof(value) - 2)) - 1))
#define MIN_TAGGED_LONG ((long) -(1L << (8 * sizeof(value) - 2)))
#define INT_TO_VAL(x)      (((int)(x) << 1) + 1)
#define VAL_TO_INT(x) ((int) VAL_TO_LONG(x))

/* Structure of the header:

For 16-bit and 32-bit architectures:
     +--------+-------+-----+
     | wosize | color | tag |
     +--------+-------+-----+
bits  31    10 9     8 7   0

For 64-bit architectures:

     +--------+-------+-----+
     | wosize | color | tag |
     +--------+-------+-----+
bits  63    10 9     8 7   0

*/

#define Tag_hd(hd) ((tag_t) ((hd) & 0xFF))
#define Wosize_hd(hd) ((mlsize_t) ((hd) >> 10))

#define Hd_val(val) (((header_t *) (val)) [-1])        /* Also an l-value. */
#define Hd_op(op) (Hd_val (op))                        /* Also an l-value. */
#define Hd_bp(bp) (Hd_val (bp))                        /* Also an l-value. */
#define Hd_hp(hp) (* ((header_t *) (hp)))              /* Also an l-value. */
#define Hp_val(val) ((char *) (((header_t *) (val)) - 1))
#define Hp_op(op) (Hp_val (op))
#define Hp_bp(bp) (Hp_val (bp))
#define Val_op(op) ((value) (op))
#define Val_hp(hp) ((value) (((header_t *) (hp)) + 1))
#define Op_hp(hp) ((value *) Val_hp (hp))
#define Bp_hp(hp) ((char *) Val_hp (hp))

#define Num_tags (1 << 8)

#define Max_wosize WORD_MAXSIZE

#define Wosize_val(val) (Wosize_hd (Hd_val (val)))
#define Wosize_op(op) (Wosize_val (op))
#define Wosize_bp(bp) (Wosize_val (bp))
#define Wosize_hp(hp) (Wosize_hd (Hd_hp (hp)))
#define Whsize_wosize(sz) ((sz) + 1)
#define Wosize_whsize(sz) ((sz) - 1)
#define Wosize_bhsize(sz) ((sz) / sizeof (value) - 1)
#define Bsize_wsize(sz) ((sz) * sizeof (value))
#define Wsize_bsize(sz) ((sz) / sizeof (value))
#define Bhsize_wosize(sz) (Bsize_wsize (Whsize_wosize (sz)))
#define Bhsize_bosize(sz) ((sz) + sizeof (header_t))
#define Bosize_val(val) (Bsize_wsize (Wosize_val (val)))
#define Bosize_op(op) (Bosize_val (Val_op (op)))
#define Bosize_bp(bp) (Bosize_val (Val_bp (bp)))
#define Bosize_hd(hd) (Bsize_wsize (Wosize_hd (hd)))
#define Whsize_hp(hp) (Whsize_wosize (Wosize_hp (hp)))
#define Whsize_val(val) (Whsize_hp (Hp_val (val)))
#define Whsize_bp(bp) (Whsize_val (Val_bp (bp)))
#define Whsize_hd(hd) (Whsize_wosize (Wosize_hd (hd)))
#define Bhsize_hp(hp) (Bsize_wsize (Whsize_hp (hp)))
#define Bhsize_hd(hd) (Bsize_wsize (Whsize_hd (hd)))

#ifdef WORDS_BIGENDIAN
#define Tag_val(val) (((unsigned char *) (val)) [-1])
                                                 /* Also an l-value. */
#define Tag_hp(hp) (((unsigned char *) (hp)) [sizeof(value)-1])
                                                 /* Also an l-value. */
#else
#define Tag_val(val) (((unsigned char *) (val)) [-sizeof(value)])
                                                 /* Also an l-value. */
#define Tag_hp(hp) (((unsigned char *) (hp)) [0])
                                                 /* Also an l-value. */
#endif

/* The tag values MUST AGREE with compiler/Config.mlp: */

/* The Lowest tag for blocks containing no value. */
#define No_scan_tag (Num_tags - 5)


/* 1- If tag < No_scan_tag : a tuple of fields.  */

/* Pointer to the first field. */
#define Op_val(x) ((value *) (x))
/* Fields are numbered from 0. */
#define Field(x, i) (((value *)(x)) [i])           /* Also an l-value. */

/* A sequence of bytecodes */
typedef unsigned char *bytecode_t;

#define Closure_wosize 2
#define Closure_tag (No_scan_tag - 2)
#define Code_val(val) (((bytecode_t *) (val)) [0])     /* Also an l-value. */
#define Env_val(val) (Field(val, 1))               /* Also an l-value. */

/* --- Reference cells are used in Moscow SML --- */

#define Reference_tag (No_scan_tag - 1)

/* --- --- */


/* 2- If tag >= No_scan_tag : a sequence of bytes. */

/* Pointer to the first byte */
#define Bp_val(v) ((char *) (v))
#define Val_bp(p) ((value) (p))
/* Bytes are numbered from 0. */
#define Byte(x, i) (((char *) (x)) [i])            /* Also an l-value. */
#define Byte_u(x, i) (((unsigned char *) (x)) [i]) /* Also an l-value. */

/* Arrays of weak pointers.  Just like abstract things, but the GC will
   reset each cell (during the weak phase, between marking and sweeping)
   as the pointed-to object gets deallocated.
*/
#define Weak_tag No_scan_tag

/* Abstract things.  Their contents is not traced by the GC; therefore they
   must not contain any [value].
*/
#define Abstract_tag (No_scan_tag + 1)

/* Strings. */
#define String_tag (No_scan_tag + 2)
#define String_val(x) ((char *) Bp_val(x))

/* Floating-point numbers. */
#define Double_tag (No_scan_tag + 3)
#define Double_wosize ((sizeof(double) / sizeof(value)))
#ifndef ALIGN_DOUBLE
#define Double_val(v) (* (double *) (v))
#else
extern double Double_val (value);
#endif
void Store_double_val (value,double);

/* Finalized things.  Just like abstract things, but the GC will call the
   [Final_fun] before deallocation.
*/
#define Final_tag (No_scan_tag + 4)
typedef void (*final_fun) (value);
#define Final_fun(val) (((final_fun *) (val)) [0]) /* Also an l-value. */


/* 3- Atoms are 0-tuples.  They are statically allocated once and for all. */

extern header_t first_atoms[];
#define Atom(tag) (Val_hp (&(first_atoms [tag])))
#define Is_atom(v) (v >= Atom(0) && v <= Atom(255))

/* Booleans are atoms tagged 0 or 1 */

#define Val_bool(x) Atom((x) != 0)
#define Bool_val(x) Tag_val(x)
#define Val_false Atom(0)
#define Val_true Atom(1)

/* The unit value is the atom tagged 0 */

#define Val_unit Atom(0)

/*  SML option values: Must match compiler/Types.sml: */

#define NONE Atom(0)
#define SOMEtag (1)

#endif /* _mlvalues_ */

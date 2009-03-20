#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "str.h"

value compare(value, value);
value equal(value, value);
value notequal(value, value);
value lessthan(value, value);
value lessequal(value, value);
value greaterthan(value, value);
value greaterequal(value, value);

/* Structural comparison on trees.
   May loop on cyclic structures. */

static long compare_val(value v1, value v2)
{
	tag_t t1, t2;
	mlsize_t len1, len2, len;
	unsigned char *c1, *c2;
	double d1, d2;
	value *p1, *p2;
	long res;

tailcall:
	if (v1 == v2) {
		return 0;
	}

	if (IS_LONG(v1) || IS_LONG(v2)) {
		return VAL_TO_LONG(v1) - VAL_TO_LONG(v2);
	}

	/* If one of the objects is outside the heap (but is not an atom),
	   use address comparison. */
	if ((!Is_atom(v1) && !Is_young(v1) && !Is_in_heap(v1)) ||
	    (!Is_atom(v2) && !Is_young(v2) && !Is_in_heap(v2))) {
		return v1 - v2;
	}

	t1 = Tag_val(v1);
	t2 = Tag_val(v2);
	if (t1 != t2) {
		return (long)t1 - (long)t2;
	}

	switch(t1) {
	case String_tag:
		len1 = string_length(v1);
		len2 = string_length(v2);
		len = (len1 <= len2 ? len1 : len2);
		c1 = (unsigned char *) String_val(v1);
		c2 = (unsigned char *) String_val(v2);
		for (; len > 0; len--) {
			if (*c1 != *c2) {
				return (long)*c1 - (long)*c2;
			}
			c1++;
			c2++;
		}
		return (long) (len1 - len2);
	case Double_tag:
		d1 = Double_val(v1);
		d2 = Double_val(v2);
		if (d1 == d2) { /* This comparison is unsafe. We should not allow it */
			return 0;
		}else if (d1 < d2) {
			return -1;
		} else {
			return 1;
		}
	case Abstract_tag:
	case Final_tag:
		invalid_argument("equal: abstract value");
	case Closure_tag:
		invalid_argument("equal: functional value");
	default:
		len1 = Wosize_val(v1);
		len2 = Wosize_val(v2);

		if (len1 != len2) {
			return (long) (len1 - len2);
		}
		for(p1 = Op_val(v1), p2 = Op_val(v2);
		    len1 > 1;
		    len1--, p1++, p2++) {
			res = compare_val(*p1, *p2);
			if (res != 0) return res;
		}
		v1 = *p1;
		v2 = *p2;
		goto tailcall;
	}
}

value compare(value v1, value v2)
{
  return LONG_TO_VAL(compare_val(v1, v2));
}

value equal(value v1, value v2)
{
  return Atom(compare_val(v1, v2) == 0);
}

value notequal(value v1, value v2)
{
  return Atom(compare_val(v1, v2) != 0);
}

value lessthan(value v1, value v2)
{
  return Atom(compare_val(v1, v2) < 0);
}

value lessequal(value v1, value v2)
{
  return Atom(compare_val(v1, v2) <= 0);
}

value greaterthan(value v1, value v2)
{
  return Atom(compare_val(v1, v2) > 0);
}

value greaterequal(value v1, value v2)
{
  return Atom(compare_val(v1, v2) >= 0);
}


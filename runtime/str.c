/* Operations on strings */

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "alloc.h"
#include "debugger.h"
#include "mlvalues.h"
#include "str.h"

static unsigned char printable_chars_ascii[] = /* 0x20-0x7E */
  "\000\000\000\000\377\377\377\377\377\377\377\377\377\377\377\177\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000";
static unsigned char printable_chars_iso[] = /* 0x20-0x7E 0xA1-0xFF */
  "\000\000\000\000\377\377\377\377\377\377\377\377\377\377\377\177\000\000\000\000\376\377\377\377\377\377\377\377\377\377\377\377";

mlsize_t string_length(value s)
{
  mlsize_t temp = Bosize_val(s) - 1;
  assert (Byte (s, temp - Byte (s, temp)) == 0);
  return temp - ((unsigned) Byte (s, temp));
}

value create_string(value len)
{
  return alloc_string(VAL_TO_LONG(len));
}

value compare_strings(value s1, value s2)
{
  mlsize_t len1, len2;
  mlsize_t len;
  unsigned char * p1, * p2;

  len1 = string_length(s1);
  len2 = string_length(s2);
  for (len = (len1 <= len2 ? len1 : len2),
         p1 = (unsigned char *) String_val(s1),
         p2 = (unsigned char *) String_val(s2);
       len > 0;
       len--, p1++, p2++)
    if (*p1 != *p2)
      return (*p1 < *p2 ? LONG_TO_VAL(-1) : LONG_TO_VAL(1));
  if (len1 == len2)
    return LONG_TO_VAL(0);
  else if (len1 < len2)
    return LONG_TO_VAL(-2);
  else
    return LONG_TO_VAL(2);
}

value blit_string(value s1, value offset1, value s2, value offset2, value len)
{
  memmove(&Byte(s2, VAL_TO_LONG(offset2)), &Byte(s1, VAL_TO_LONG(offset1)), VAL_TO_INT(len));
  return Atom(0);
}

value fill_string(value s, value offset, value len, value init)
{
  char * p;
  mlsize_t n;
  char c;

  c = VAL_TO_LONG(init);
  for(p = &Byte(s, VAL_TO_LONG(offset)), n = VAL_TO_LONG(len);
      n > 0; n--, p++)
    *p = c;
  return Atom(0);
}

value is_printable(value chr)
{
  int c;

  static int iso_charset = -1;
  unsigned char * printable_chars;

  if (iso_charset == -1) {
    char * lc_ctype = (char *) getenv("LC_CTYPE");
    if (lc_ctype != 0 && strcmp(lc_ctype, "iso_8859_1") == 0)
      iso_charset = 1;
    else
      iso_charset = 0;
  }
  printable_chars = iso_charset ? printable_chars_iso : printable_chars_ascii;

  c = VAL_TO_INT(chr);
  return Val_bool(printable_chars[c >> 3] & (1 << (c & 7)));
}

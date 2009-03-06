#ifndef _str_
#define _str_


#include "misc.h"

extern mlsize_t string_length (value);
extern value compare_strings (value, value);
extern value create_string(value);
extern value blit_string(value, value, value, value, value);
extern value fill_string(value, value, value, value);
extern value is_printable(value);

#endif /* _str_ */

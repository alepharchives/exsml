#ifndef _alloc_
#define _alloc_

#include "misc.h"
#include "mlvalues.h"

extern value alloc(mlsize_t, tag_t);
extern value alloc_tuple(mlsize_t);
extern value alloc_string(mlsize_t);
extern value alloc_final(mlsize_t, final_fun, mlsize_t, mlsize_t);
extern value copy_string(char *);
extern value copy_string_array(char **);
extern value copy_double(double);
extern value alloc_array(value (*funct) (char *), char ** array);
extern int convert_flag_list(value, int *);


#endif /* _alloc_ */

/* dynlib.c -- foreign function interface, dynamically loadable libraries */

#include <dlfcn.h>

#include "mlvalues.h"
#include "fail.h"

/* Ken Larsen (kla@it.dtu.dk) 1998-01-08 */
value dynlib_dlopen(value libname, value flagval) /* ML */
{
	void *handle;

	int mlflags = VAL_TO_LONG(flagval);
	int cflags;
	if (1 & mlflags)
		cflags = RTLD_NOW;
	else
		cflags = RTLD_LAZY;

	handle = dlopen (String_val(libname), cflags);
	if (!handle) {
		failwith(dlerror());
	}

	/* Since handle is a void pointer, we can just cast it to value */
	return (value) handle;
}

value dynlib_dlclose(value handle) /* ML */
{
  dlclose((void *) handle);

  return Val_unit;
}

value dynlib_dlsym(value handle, value sym) /* ML */
{
  void *symhdl;

  symhdl = dlsym((void *) handle, String_val(sym));
  {
    char *error = dlerror();
    if (error != NULL)
      failwith(error);
  }

  return (value) symhdl;
}

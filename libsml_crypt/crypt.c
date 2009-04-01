/* File mosml/src/dynlibs/crypt/crypt.c -- foreign function interface
   The C side of things: how to define the C function to be invoked from SML
 */

#define _XOPEN_SOURCE
#include <unistd.h>		/* For crypt       */

/* Moscow ML specific includes: */

#include <mlvalues.h>		/* For String_val  */
#include <alloc.h>		/* For copy_string */

value ml_crypt(value, value);

/* Type on the SML side: string -> string -> string */

value ml_crypt(value mlkey, value mlsalt)
{
	/* Obtain pointers to the SML strings mlkey and mlsalt in the SML heap: */
	char *key  = String_val(mlkey);
	char *salt = String_val(mlsalt);

	/* Invoke C library function: */
	char *res = (char*)crypt(key, salt);

	/* The result is a pointer to a null-terminated C string.    */
	/* Copy it to a fresh string in the ML heap and return that: */
	return copy_string(res);
}

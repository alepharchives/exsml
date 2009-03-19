/* unix.c -- Moscow ML C primitives available only under Unix */

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <sys/param.h>
#include <sys/utsname.h>
#include <unistd.h>

#include "attributes.h"
#include "mlvalues.h"
#include "fail.h"
#include "memory.h"
#include "str.h"
#include "alloc.h"

value sml_realpath(value);
value sml_uname(value);
value sml_islink(value);
value sml_readlink(value);
value sml_devinode(value);
value sml_setdisk(value);

value sml_realpath(value v)
{
  char buffer[MAXPATHLEN];
  char *result;

  result = realpath(String_val(v), buffer);
  if (result == NULL)
      failwith("realpath");
  return copy_string(result);
}

value sml_uname(value UNUSED(v))
{
  struct utsname buf;
  value res;
  PUSH_ROOTS(r, 1);

  if (uname(&buf) == -1)
      failwith("uname");
  res = alloc (3, 0);
  r[0] = res;
  Field (r[0], 0) = copy_string (buf.machine);
  Field (r[0], 1) = copy_string (buf.sysname);
  Field (r[0], 2) = copy_string (buf.release);
  res = r[0];
  POP_ROOTS();
  return res;
}

value sml_islink(value path)
{
  struct stat buf;

  if (lstat(String_val(path), &buf) == -1)
      failwith("lstat");
  return Val_bool((S_IFLNK & buf.st_mode) == S_IFLNK);
}

value sml_readlink(value v)
{
  char buffer[MAXPATHLEN];
  long result;

  result = readlink(String_val(v), buffer, MAXPATHLEN);
  if (result == -1 || result >= MAXPATHLEN)
      failwith("readlink");
  buffer[result] = '\0';
  return copy_string(buffer);
}

value sml_devinode(value path)
{
  struct stat buf;
  double dev_inode;

  if (stat(String_val(path), &buf) == -1)
      failwith("stat");
  /* Combine the device and inode into a real number */
  dev_inode = ((double)buf.st_dev * (1 << 17)) + (double)buf.st_ino;
  return copy_double(dev_inode);
}

value sml_setdisk(value UNUSED(volno))
{
  failwith("setdisk");
  return Val_unit;		/* Can't reach return */
}

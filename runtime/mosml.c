/* Moscow ML primitives */

#include <math.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <ctype.h>

#include <sys/time.h>
#include <sys/times.h>
#include <sys/resource.h>
#include <dirent.h>
#include <sys/param.h>
#include <unistd.h>
#include <utime.h>

#include <assert.h>

#include "mlvalues.h"
#include "fail.h"
#include "memory.h"
#include "str.h"
#include "runtime.h"
#include "alloc.h"
#include "major_gc.h"
#include "intext.h"
#include "debugger.h"
#include "interp.h"
#include "global_tbl.h"
#include "mosml.h"

value sml_equal(value, value);
value sml_not_equal(value, value);
value sml_system(value);
value sml_abs_int(value);
value sml_floor(value);
value sml_ceil(value);
value sml_round(value);
value sml_trunc(value);
value sml_abs_real(value);
value sml_sqrt(value);
value sml_sin(value);
value sml_cos(value);
value sml_exp(value);
value sml_ln(value);
unsigned long scandec(char *, unsigned long);
unsigned long scanhex(char *, unsigned long);
value sml_int_of_string(value);
value sml_concat(value, value);
value sml_chr(value);
value sml_ord(value);
value sml_float_of_string(value);
static int countChar(int, char *);
static void mkSMLMinus(char *);
value sml_string_of_int(value);
void string_of_float_aux(char *, double);
value sml_string_of_float(value);
value sml_makestring_of_char(value);
value sml_makestring_of_string(value);
value sml_getrealtime(value);
value sml_getrutime(value);
value sml_errno(value);
value sml_getdir(value);
value sml_mkdir(value);
value sml_rmdir(value);
value sml_opendir(value);
value sml_rewinddir(value);
value sml_readdir(value);
value sml_closedir(value);
value sml_isdir(value);
value sml_modtime(value);
value sml_settime(value, value);
value sml_access(value, value);
value sml_tmpnam(value v);
value sml_errormsg(value);
value sml_asin(value);
value sml_acos(value);
value sml_atan2(value, value);
value sml_pow(value, value);
value sml_localtime(value);

void fatal(char *s) {
	if (s == NULL) {
		perror(strerror(errno));
		exit(EXIT_FAILURE);
	} else {
		perror(s);
		exit(EXIT_FAILURE);
	}
}

#define Raise_float_if(cond) \
  if( cond )					\
    { raiseprimitive0(float_exn); }

#define Check_float(dval)					\
   Raise_float_if( (dval > maxdouble) || (dval < -maxdouble) )

/* Structural equality on trees. */
/* Note how reference cells are treated! */

static int sml_equal_aux(value v1, value v2)
{
  mlsize_t i;
  value * p1, * p2;

 again:
  if (v1 == v2) return 1;
  if (IS_LONG(v1) || IS_LONG(v2)) return 0;
  if (!Is_in_heap(v1) && !Is_young(v1)) return 0;
  if (!Is_in_heap(v2) && !Is_young(v2)) return 0;
  if (Tag_val(v1) != Tag_val(v2)) return 0;
  switch(Tag_val(v1)) {
  case String_tag:
    return (compare_strings(v1, v2) == LONG_TO_VAL(0));
  case Double_tag:
    return (Double_val(v1) == Double_val(v2));
  case Reference_tag:  /* Different reference cells are not equal! */
  case Abstract_tag:
  case Final_tag:
    return 0;
  case Closure_tag:
    invalid_argument("sml_equal: functional value");
  default:
    i = Wosize_val(v1);
    if (i != Wosize_val(v2)) return 0;
    for(p1 = Op_val(v1), p2 = Op_val(v2);
        i > 1;
        i--, p1++, p2++)
      if (!sml_equal_aux(*p1, *p2)) return 0;
    v1 = *p1;
    v2 = *p2;                   /* Tail-call */
    goto again;
  }
}

value sml_equal(value v1, value v2)
{
  return Atom(sml_equal_aux(v1,v2));
}

value sml_not_equal(value v1, value v2)
{
  return Atom(!sml_equal_aux(v1,v2));
}

value sml_system(value cmd)
{
  return INT_TO_VAL(system(String_val(cmd)));
}

value sml_abs_int(value x)
{ value tmp, v;
  tmp = VAL_TO_LONG(x);
  if( tmp < 0 ) tmp = -tmp;
  v = LONG_TO_VAL(tmp);
  if( VAL_TO_LONG(v) != tmp )
    raise_overflow();
  return v;
}

value sml_floor(value f)
{ double r;
  long i;
  value v;
  r = Double_val(f);
  if( r >= 0.0 )
    { if( r >= ((double) MAX_TAGGED_LONG + 1) ) goto raise_floor;
      i = (long) r;
    }
  else
    {
      if( r < ((double) MIN_TAGGED_LONG) ) goto raise_floor;
      i = (long) r;
      if( r < ((double) i) ) i -= 1;
    }
  v = LONG_TO_VAL(i);
  if( VAL_TO_LONG(v) != i )  goto raise_floor;
  return v;

raise_floor:
    raise_overflow();
    return Val_unit;		/* Can't reach return */
}

value sml_ceil(value f)
{ double r;
  long i;
  value v;
  r = Double_val(f);
  if( r >= 0.0 )
    { if( r > ((double) (MAX_TAGGED_LONG)) ) goto raise_ceil;
      i = (long) r;
      if( r > ((double) i) ) i += 1;
    }
  else
    { if( r <= ((double) (MIN_TAGGED_LONG-1)) ) goto raise_ceil;
      i = (long) r;
    }
  v = LONG_TO_VAL(i);
  if( VAL_TO_LONG(v) != i )  goto raise_ceil;
  return v;

raise_ceil:
    raise_overflow();
    return Val_unit;		/* Can't reach return */
}

value sml_round(value f)
{
	double r;
	long i;
	value v;

	r = rint(Double_val(f));
	if ((r > (double) (MAX_TAGGED_LONG))
	    || (r < (double)(MIN_TAGGED_LONG)))
		goto raise_round;

	i = (long) r;
	v = LONG_TO_VAL(i);

	return v;

raise_round:
	raise_overflow();
	return Val_unit;		/* Can't reach return */
}

value sml_trunc(value f)
{ double r;
  long i;
  value v;
  r = Double_val(f);
  if ((r >= (double) (MAX_TAGGED_LONG+1)) || (r <= (double)(MIN_TAGGED_LONG-1)))
    goto raise_trunc;
  i = (long) r;
  v = LONG_TO_VAL(i);
  return v;

raise_trunc:
  raise_overflow();
  return Val_unit;		/* Can't reach return */
}

value sml_abs_real(value f)
{ double r;
  float_exn = SYS__EXN_OVERFLOW;
  r = Double_val(f);
  if( r >= 0.0 )
    return f;
  else
    r = -r;
  Check_float(r);
  return copy_double(r);
}

value sml_sqrt(value f)
{ double r;
  float_exn = SYS__EXN_DOMAIN;
  r = Double_val(f);
  Raise_float_if( r < 0.0 );
  r = sqrt(r);
  Check_float(r);
  return copy_double(r);
}

value sml_sin(value f)
{ double r;
  r = Double_val(f);
  r = sin(r);
  if( r != r || r > 1.0 || r < -1.0 )
    failwith("sin: argument too large");
  return copy_double(r);
}

value sml_cos(value f)
{ double r;
  r = Double_val(f);
  r = cos(r);
  if( r != r || r > 1.0 || r < -1.0 )
    failwith("cos: argument too large");
  return copy_double(r);
}

value sml_exp(value f)
{ double r;
  float_exn = SYS__EXN_OVERFLOW;
  r = exp(Double_val(f));
  Check_float(r);
  return copy_double(r);
}

value sml_ln(value f)
{ double r;
  float_exn = SYS__EXN_DOMAIN;
  r = Double_val(f);
  Raise_float_if( r <= 0.0 );
  r = log(r);
  Check_float(r);
  return copy_double(r);
}

unsigned long scandec(char * p, unsigned long max)
{ unsigned long res;
  int c, d;
  res = 0;
  while (1) {
    c = *p;
    if (c >= '0' && c <= '9')
      d = c - '0';
    else
      break;
    if( (res > (max/10)) ||
        ((res == (max/10) && ((max % 10) <= d))) )
      goto raise_failure;
    res = 10 * res + d;
    p++;
  }
  if (*p != 0)
    goto raise_failure;
  return res;

  raise_failure:
    failwith("scandec");
    return 0;		/* Can't reach return */
}

unsigned long scanhex(char * p, unsigned long max)
{ unsigned long res;
  int c, d;
  res = 0;
  while (1) {
    c = toupper(*p);
    if (c >= '0' && c <= '9')
      d = c - '0';
    else if (c >= 'A' && c <= 'F')
      d = c + (10 - 'A');
    else
      break;
    if( (res > (max/16)) ||
        ((res == (max/16) && ((max % 16) <= d))) )
      goto raise_failure;
    res = 16 * res + d;
    p++;
  }
  if (*p != 0)
    goto raise_failure;
  return res;

  raise_failure:
    failwith("scanhex");
    return 0;		/* Can't reach return */
}

value sml_int_of_string(value s)
{ value v;
  long res;
  int sign;
  char * p;

  p = String_val(s);
  sign = 1;
  if (*p == '~') {
    sign = -1;
    p++;
  }
  res = sign * scandec(p, (unsigned long) MIN_TAGGED_LONG);
  v = LONG_TO_VAL(res);
  if( VAL_TO_LONG(v) != res )
    goto raise_failure;
  return v;

  raise_failure:
    failwith("sml_int_of_string");
    return Val_unit;		/* Can't reach return */
}

value sml_concat(value s1, value s2)
{
  mlsize_t len1, len2, len;
  value s;
  len1 = string_length(s1);
  if (len1 == 0)
    return s2;
  len2 = string_length(s2);
  if (len2 == 0)
    return s1;
  {
    PUSH_ROOTS(r, 2);
    r[0] = s1;
    r[1] = s2;
    len = len1 + len2;
    if( (len + sizeof (value)) / sizeof (value) > Max_wosize )
      raiseprimitive0(SYS__EXN_SIZE);
    s = alloc_string(len);
    memmove(&Byte(s, 0), &Byte(r[0], 0), len1);
    memmove(&Byte(s, len1), &Byte(r[1], 0), len2);
    POP_ROOTS();
    return s;
  }
}

value sml_chr(value v)
{
  long i;
  value s;
  i = VAL_TO_LONG(v);
  if( i < 0 || i > 255 )
    raiseprimitive0(SYS__EXN_CHR);
  s = alloc_string(1);
  *(&Byte(s,0)) = (unsigned char) i;
  return s;
}

value sml_ord(value s)
{
  long i;
  if( string_length(s) == 0 )
    raiseprimitive0(SYS__EXN_ORD);
  i = (unsigned char) *(&Byte(s,0));
  return LONG_TO_VAL(i);
}

value sml_float_of_string(value s)
{

  char buff[64];
  mlsize_t len;
  int i, e_len;
  char c;
  char *p;
  double r;

  len = string_length(s);
  if(len > sizeof(buff) - 1)
    failwith("sml_float_of_string: argument too large");
  p = String_val(s);
  e_len = -1;
  for (i = 0; i<len; i++) {
    c = *p++;
    switch( c ) {
        case '~':
          buff[i] = '-'; break;
        case 'E':
          buff[i] = 'e'; e_len = 0; break;
        default:
          buff[i] = c;
          if( e_len >= 0 ) e_len++;
          Raise_float_if( e_len > 5 )
          break;
    }
  }
  buff[len] = 0;
  r = atof(buff);
  if( (r > maxdouble) || (r < -maxdouble) )
    failwith("sml_float_of_string: result too large");
  return copy_double(r);
}

static int countChar(int c, char * s)
{
  char *p; int count;

  count = 0;
  for( p=s; *p != '\0'; p++ ) {
    if( *p == c ) count++;
  }
  return count;
}

/* Here we remove all '+', and replace '-' and 'e' with '~' and 'E'.
   Also, drop a single leading zero from the exponent. */

static void mkSMLMinus(char * s)
{
  char *p = s, *q = s;
  int justafterexp = 0;		/* After exponent but before digits */

  for ( ; *p != '\0'; p++) {
    switch( *p ) {
    case '+': break;
    case '-': *q++ = '~'; break;
    case 'e': *q++ = 'E'; justafterexp = 1; break;
    case '0':
      if (!justafterexp)	/* Don't copy zero just after exponent */
	*q++ = '0';
      justafterexp = 0;
      break;
    default: *q++ = *p; justafterexp = 0; break;
    }
  }
  *q = '\0';
  return;
}

value sml_string_of_int(value arg)
{
  char format_buffer[32];

  sprintf(format_buffer, "%ld", VAL_TO_LONG(arg));
  mkSMLMinus(format_buffer);
  return copy_string(format_buffer);
}

/* Convert real x to SMLish format in format_buffer */

void string_of_float_aux(char* format_buffer, double x)
{
  sprintf(format_buffer, "%.12g", x);
  mkSMLMinus(format_buffer);
  if( countChar('.', format_buffer) == 0 &&
      countChar('E', format_buffer) == 0 )
    strcat(format_buffer, ".0");
}

value sml_string_of_float(value arg)
{
  char format_buffer[64];
  string_of_float_aux(format_buffer, Double_val(arg));
  return copy_string(format_buffer);
}

value sml_makestring_of_char(value arg)
{
  unsigned char c;
  char buff[8];

  c = VAL_TO_INT(arg);
  switch (c)
    {
    case '"':   return copy_string("#\"\\\"\"");
    case '\\':  return copy_string("#\"\\\\\"");
    case '\a':  return copy_string("#\"\\a\"");
    case '\b':  return copy_string("#\"\\b\"");
    case '\t':  return copy_string("#\"\\t\"");
    case '\n':  return copy_string("#\"\\n\"");
    case '\v':  return copy_string("#\"\\v\"");
    case '\f':  return copy_string("#\"\\f\"");
    case '\r':  return copy_string("#\"\\r\"");
    default:
      buff[0] = '#'; buff[1] = '"';
      if( c <= 31 ) {
        buff[2] = '\\'; buff[3] = '^'; buff[4] = c + 64;
        buff[5] = '"'; buff[6] = 0;
        return copy_string(buff);
        }
      else if( (32 <= c && c <= 126) || (128 <= c && c <= 254) ) {
        buff[2] = c; buff[3] = '"'; buff[4] = 0;
        return copy_string(buff);
        }
      else {
        buff[2] = '\\';
        buff[3] = 48 + c / 100;
        buff[4] = 48 + (c / 10) % 10;
        buff[5] = 48 + c % 10;
        buff[6] = '"';
        buff[7] = 0;
        return copy_string(buff);
        }
    }
}

value sml_makestring_of_string(value arg)
{
  mlsize_t arg_len, len, i;
  value res;
  char *a; char *b;
  unsigned char c;
  PUSH_ROOTS(r, 1);

  r[0] = arg;
  arg_len = string_length(arg);

  a = String_val(r[0]);
  len = 0;
  for( i = 0; i < arg_len; i++ ) {
    c = a[i];
    switch (c)
      {
      case '"': case '\\':
      case '\a': case '\b': case '\t': case '\n': case '\v':
      case '\f': case '\r':
        len += 2; break;
      default:
        if( c <= 31)
          len += 3;
        else if( (32 <= c && c <= 126) || (128 <= c && c <= 254) )
          len += 1;
        else
          len += 4;
        break;
      }
    }

  if( (len + 2 + sizeof (value)) / sizeof (value) > Max_wosize )
    failwith("sml_string_for_read: result too large");
  res = alloc_string(len + 2);

  a = String_val(r[0]);
  b = String_val(res);
  *b++ = '"';
  for( i = 0; i < arg_len; i++) {
    c = a[i];
    switch (c)
      {
      case '"':   *b++ = '\\'; *b++ = '"';  break;
      case '\\':  *b++ = '\\'; *b++ = '\\'; break;
      case '\a':  *b++ = '\\'; *b++ = 'a';  break;
      case '\b':  *b++ = '\\'; *b++ = 'b';  break;
      case '\t':  *b++ = '\\'; *b++ = 't';  break;
      case '\n':  *b++ = '\\'; *b++ = 'n';  break;
      case '\v':  *b++ = '\\'; *b++ = 'v';  break;
      case '\f':  *b++ = '\\'; *b++ = 'f';  break;
      case '\r':  *b++ = '\\'; *b++ = 'r';  break;
      default:
        if( c <= 31 )
          { *b++ = '\\'; *b++ = '^'; *b++ = c + 64; break; }
        else if( (32 <= c && c <= 126) || (128 <= c && c <= 254) )
          { *b++ = c; break; }
        else
          { *b++ = '\\';
            *b++ = 48 + c / 100;
            *b++ = 48 + (c / 10) % 10;
            *b++ = 48 + c % 10;
            break; }
      }
    }
  *b++ = '"';
  POP_ROOTS();
  return res;
}

/* The following must agree with timebase in mosmllib/Time.sml: */

#define TIMEBASE (-1073741824)

/* There is another problem on the Mac: with a time base of 1904,
   most times are simply out of range of mosml integers. So, I added
   the macros below to compensate. 07Sep95 e
*/

value sml_getrealtime(value v)
{
  value res;
  struct timeval tp;

  gettimeofday(&tp, NULL);
  res = alloc (2, 0);
  Field (res, 0) = LONG_TO_VAL (tp.tv_sec+TIMEBASE);
  Field (res, 1) = LONG_TO_VAL (tp.tv_usec);
  return res;
}

value sml_getrutime(value v)
{
  value res;

  struct rusage rusages;
  getrusage(RUSAGE_SELF, &rusages);
  res = alloc (6, 0);
  Field (res, 2) = LONG_TO_VAL (rusages.ru_stime.tv_sec);
  Field (res, 3) = LONG_TO_VAL (rusages.ru_stime.tv_usec);
  Field (res, 4) = LONG_TO_VAL (rusages.ru_utime.tv_sec);
  Field (res, 5) = LONG_TO_VAL (rusages.ru_utime.tv_usec);

  Field (res, 0) = LONG_TO_VAL (gc_time.tv_sec);
  Field (res, 1) = LONG_TO_VAL (gc_time.tv_usec);

  return res;
}

value sml_errno(value arg)
{
  return LONG_TO_VAL(errno);
}

value sml_getdir(value arg)
{
 char directory[MAXPATHLEN];
 char *res;

 errno = 0;
 res = getcwd(directory, MAXPATHLEN);
 if (res == NULL)
    failwith("getcwd");
 return copy_string(directory);
}

value sml_mkdir(value path)
{
  if (mkdir(String_val(path), 0777) == -1)
      failwith("mkdir");
  return Val_unit;
}

value sml_rmdir(value path)
{
  if (rmdir(String_val(path)) == -1)
      failwith("rmdir");
  return Val_unit;
}


value sml_opendir(value path)
{
  DIR * dstr;

  dstr = opendir(String_val(path));
  if (dstr == NULL)
      failwith("opendir");
  return (value) dstr;
}

value sml_rewinddir(value v)
{
  rewinddir((DIR *) v);
  return Val_unit;
}

value sml_readdir(value v)
{
  struct dirent *direntry;

  direntry = readdir((DIR *) v);
  if (direntry == NULL)
      return copy_string("");
  return copy_string((*direntry).d_name);
}

value sml_closedir(value v)
{
  if (closedir((DIR *) v) == -1)
      failwith("closedir");
  return Val_unit;
}

value sml_isdir(value path)
{
  struct stat buf;

  if (stat(String_val(path), &buf) == -1)
      failwith("stat");
  return (Val_bool(S_ISDIR(buf.st_mode)));
}

value sml_modtime(value path)
{ struct stat buf;

  if (stat(String_val(path), &buf) == -1)
      failwith("stat");
  return (copy_double ((double) (buf.st_mtime)));
}

value sml_settime(value path, value time)
{
  struct utimbuf tbuf;

  tbuf.actime = tbuf.modtime = (long) (Double_val(time));
  if (utime(String_val(path), &tbuf) == -1)
      failwith("utime");
  return Val_unit;
}

value sml_access(value path, value permarg)
{
  long perms;
  long perm = VAL_TO_LONG(permarg);

  perms  = ((0x1 & perm) ? R_OK : 0);
  perms |= ((0x2 & perm) ? W_OK : 0);
  perms |= ((0x4 & perm) ? X_OK : 0);
  if (perms == 0) perms = F_OK;

  if (access(String_val(path), perms) == 0)
    return Val_bool(1);
  return Val_bool(0);
}

value sml_tmpnam(value v)
{
  /*  char *res;
      res = tmpnam(NULL);
      if (res == NULL)
      failwith("tmpnam");
      return copy_string(res); */
  failwith("tmpnam is dangerous");
  return Val_unit;
}

value sml_errormsg(value err)
{
  int errnum;
  errnum = VAL_TO_LONG(err);
  return copy_string(strerror(errnum));
}

value sml_asin(value f)
{ double r = Double_val(f);
  float_exn = SYS__EXN_DOMAIN;
  Raise_float_if( r < -1.0 || r > 1.0 );
  r = asin(r);
  Raise_float_if( r != r );
  return copy_double(r);
}

value sml_acos(value f)
{ double r = Double_val(f);
  float_exn = SYS__EXN_DOMAIN;
  Raise_float_if( r < -1.0 || r > 1.0 );
  r = acos(r);
  Raise_float_if( r != r );
  return copy_double(r);
}

value sml_atan2(value f1, value f2)
{ double r, r1, r2;
  float_exn = SYS__EXN_DOMAIN;
  r1 = Double_val(f1);
  r2 = Double_val(f2);
  if (r1 == 0.0 && r2 == 0.0)
    return copy_double(0.0);
  r = atan2(r1, r2);
  Check_float(r);
  Raise_float_if( r != r );
  return copy_double(r);
}

value sml_pow(value f1, value f2)
{ double r, r1, r2;
  float_exn = SYS__EXN_DOMAIN;
  r1 = Double_val(f1);
  r2 = Double_val(f2);
  if (r1 == 0.0 && r2 == 0.0)
    return copy_double(1.0);
  if (   (r1 == 0.0 && r2 < 0.0)
      || (r1 < 0.0 && (   fabs(r2) > (double) (MAX_TAGGED_LONG)
		       || r2 != (double)(long)r2)))
    raiseprimitive0(float_exn);
  r = pow(r1, r2);
  float_exn = SYS__EXN_OVERFLOW;
  Check_float(r);
  float_exn = SYS__EXN_DOMAIN;
  Raise_float_if( r != r );
  return copy_double(r);
}

value sml_localtime (value v)
{
  value res;
  struct tm *tmr;
  time_t clock = (long) (Double_val(v));
  tmr = localtime(&clock);
  res = alloc (9, 0);
  Field (res, 0) = LONG_TO_VAL ((*tmr).tm_hour);
  Field (res, 1) = LONG_TO_VAL ((*tmr).tm_isdst);
  Field (res, 2) = LONG_TO_VAL ((*tmr).tm_mday);
  Field (res, 3) = LONG_TO_VAL ((*tmr).tm_min);
  Field (res, 4) = LONG_TO_VAL ((*tmr).tm_mon);
  Field (res, 5) = LONG_TO_VAL ((*tmr).tm_sec);
  Field (res, 6) = LONG_TO_VAL ((*tmr).tm_wday);
  Field (res, 7) = LONG_TO_VAL ((*tmr).tm_yday);
  Field (res, 8) = LONG_TO_VAL ((*tmr).tm_year);

  return res;
}

value sml_gmtime (value v)
{
  value res;
  struct tm *tmr;
  time_t clock = (long) (Double_val(v));
  tmr = gmtime(&clock);
  res = alloc (9, 0);
  Field (res, 0) = LONG_TO_VAL ((*tmr).tm_hour);
  Field (res, 1) = LONG_TO_VAL ((*tmr).tm_isdst);
  Field (res, 2) = LONG_TO_VAL ((*tmr).tm_mday);
  Field (res, 3) = LONG_TO_VAL ((*tmr).tm_min);
  Field (res, 4) = LONG_TO_VAL ((*tmr).tm_mon);
  Field (res, 5) = LONG_TO_VAL ((*tmr).tm_sec);
  Field (res, 6) = LONG_TO_VAL ((*tmr).tm_wday);
  Field (res, 7) = LONG_TO_VAL ((*tmr).tm_yday);
  Field (res, 8) = LONG_TO_VAL ((*tmr).tm_year);
  return res;
}

value sml_mktime (value v)
{
  struct tm tmr;

  tmr.tm_hour  = VAL_TO_LONG(Field (v, 0));
  tmr.tm_isdst = VAL_TO_LONG(Field (v, 1));
  tmr.tm_mday  = VAL_TO_LONG(Field (v, 2));
  tmr.tm_min   = VAL_TO_LONG(Field (v, 3));
  tmr.tm_mon   = VAL_TO_LONG(Field (v, 4));
  tmr.tm_sec   = VAL_TO_LONG(Field (v, 5));
  tmr.tm_wday  = VAL_TO_LONG(Field (v, 6));
  tmr.tm_yday  = VAL_TO_LONG(Field (v, 7));
  tmr.tm_year  = VAL_TO_LONG(Field (v, 8));

  return copy_double((double) mktime(&tmr));
}

value sml_asctime (value v)
{
  struct tm tmr;
  char *res;

  tmr.tm_hour  = VAL_TO_LONG(Field (v, 0));
  tmr.tm_isdst = VAL_TO_LONG(Field (v, 1));
  tmr.tm_mday  = VAL_TO_LONG(Field (v, 2));
  tmr.tm_min   = VAL_TO_LONG(Field (v, 3));
  tmr.tm_mon   = VAL_TO_LONG(Field (v, 4));
  tmr.tm_sec   = VAL_TO_LONG(Field (v, 5));
  tmr.tm_wday  = VAL_TO_LONG(Field (v, 6));
  tmr.tm_yday  = VAL_TO_LONG(Field (v, 7));
  tmr.tm_year  = VAL_TO_LONG(Field (v, 8));

  res = asctime(&tmr);
  if (res == NULL)
    failwith("asctime");
  return copy_string(res);
}

value sml_strftime (value fmt, value v)
{
  struct tm tmr;
#define BUFSIZE 256
  char buf[BUFSIZE];
  long ressize;

  tmr.tm_hour  = VAL_TO_LONG(Field (v, 0));
  tmr.tm_isdst = VAL_TO_LONG(Field (v, 1));
  tmr.tm_mday  = VAL_TO_LONG(Field (v, 2));
  tmr.tm_min   = VAL_TO_LONG(Field (v, 3));
  tmr.tm_mon   = VAL_TO_LONG(Field (v, 4));
  tmr.tm_sec   = VAL_TO_LONG(Field (v, 5));
  tmr.tm_wday  = VAL_TO_LONG(Field (v, 6));
  tmr.tm_yday  = VAL_TO_LONG(Field (v, 7));
  tmr.tm_year  = VAL_TO_LONG(Field (v, 8));

  ressize = strftime(buf, BUFSIZE, String_val(fmt), &tmr);
  if (ressize == 0 || ressize == BUFSIZE)
    failwith("strftime");
  return copy_string(buf);
#undef BUFSIZE
}

value sml_general_string_of_float(value fmt, value arg)
{
#define BUFSIZE 512
  char format_buffer[BUFSIZE];

  /* Unfortunately there seems to be no way to ensure that this does not
   * crash by overflowing the format_buffer (e.g. when specifying a huge
   * number of decimal digits in the fixed-point format).  Well, we might
   * use snprintf if universally supported?
   */

  double x = Double_val(arg);
  if (x == -0.0) x = 0.0;
  sprintf(format_buffer, String_val(fmt), x);

  mkSMLMinus(format_buffer);
  return copy_string(format_buffer);
#undef BUFSIZE
}

value sml_filesize(value path)
{ struct stat buf;

  if (stat(String_val(path), &buf) == -1)
      failwith("stat");
  return (LONG_TO_VAL (buf.st_size));
}

value sml_int_of_hex(value s)
{ value v;
  long res;
  int sign;
  char * p;

  /* The argument s has form [~]?0x[0-9a-fA-F]+ */

  p = String_val(s);
  sign = 1;
  if (*p == '~') {
    sign = -1;
    p++;
  }
  /* skip 0x in s */
  p += 2;

  res = sign * scanhex(p, (unsigned long)MIN_TAGGED_LONG);
  v = LONG_TO_VAL(res);
  if( VAL_TO_LONG(v) != res )
    goto raise_failure;
  return v;

  raise_failure:
    failwith("sml_int_of_hex");
    return Val_unit;		/* Can't reach return */
}

value sml_word_of_hex(value s)
{ value v;
  long res;
  char * p;

  /* The argument s has form 0wx[0-9a-fA-F]+ */

  p = String_val(s);
  /* skip 0wx in s */
  p += 3;

  res = scanhex(p, 2 * (unsigned long)MIN_TAGGED_LONG);
  v = LONG_TO_VAL((long)res);
  return v;
}

value sml_word_of_dec(value s)
{ value v;
  long res;
  char * p;

  /* The argument s has form 0w[0-9]+ */
  p = String_val(s);
  /* skip 0w in s */
  p += 2;

  res = (long)scandec(p, 2 * (unsigned long)MIN_TAGGED_LONG);
  v = LONG_TO_VAL((long)res);
  return v;
}

value sml_hexstring_of_word(value arg)
{
  char format_buffer[32];

  sprintf(format_buffer, "0wx%lX", VAL_TO_LONG((unsigned long)arg));
  return copy_string(format_buffer);
}

value sml_sinh(value f)
{ double r;
  float_exn = SYS__EXN_OVERFLOW;
  r = Double_val(f);
  r = sinh(r);
  Check_float(r);
  return copy_double(r);
}

value sml_cosh(value f)
{ double r;
  float_exn = SYS__EXN_OVERFLOW;
  r = Double_val(f);
  r = cosh(r);
  Check_float(r);
  return copy_double(r);
}

value sml_tanh(value f)
{ double r;
  float_exn = SYS__EXN_DOMAIN;
  r = Double_val(f);
  r = tanh(r);
  Check_float(r);
  return copy_double(r);
}

/* A weak pointer v is dead (dangling) if NULL, or if we are in the
   weak phase and v is a white block in the heap.

   Conversely, v is live if
   * v is non-NULL
   AND
   * v isn't a block (e.g. an int or char), OR
   * v isn't in the heap (e.g. is an atom, or in the young generation), OR
   * we're in the mark phase (in which v may be resurrected by darkening), OR
   * we're in the weak phase but v has been darkened (so it will survive
     the sweep phase), OR
   * we're in the sweep phase (since the pointer hasn't been reset by the
     weak phase, v must have been dark at that time; hence v will
     not be deallocated, but sweeping may have changed its color already).
*/

int isdead(value v)
{
  return v == (value)NULL
         || (gc_phase == Phase_weak
	     && IS_BLOCK(v) && Is_in_heap(v) && Is_white_val(v));
}

value weak_sub(value arr, value index)
{
  value v = Field(arr, VAL_TO_LONG(index));
  if (isdead(v))
    failwith("Dangling weak pointer");
  else
    if (gc_phase == Phase_mark)
      darken(v);
  return v;
}

value weak_isdead(value arr, value index)
{
  return Val_bool(isdead(Field(arr, VAL_TO_LONG(index))));
}

value weak_arr(value size)
{
  value res;
  mlsize_t sz, i;

  sz = VAL_TO_LONG(size);
  if (sz == 0) return Atom(Weak_tag);
  res = alloc_shr(sz, Weak_tag);	/* Must go in the major heap */
  for (i = 0; i < sz; i++)
    Field(res, i) = (value)NULL;

  return res;
}

/* Turn an ML value into an externalized ML value (a string), a la extern.c */

value string_mlval(value val)
{
  value s;
  byteoffset_t res;

  extern_size = INITIAL_EXTERN_SIZE;
  extern_block =
    (byteoffset_t *) stat_alloc(extern_size * sizeof(unsigned long));
  extern_pos = 0;
  extern_table_size = INITIAL_EXTERN_TABLE_SIZE;
  alloc_extern_table();
  extern_table_used = 0;
  res = emit_all(val);
  stat_free((char *) extern_table);

  /* We can allocate a string in the heap since the argument value is
     not used from now on. */
  if (extern_pos == 0)
    {
      s = alloc_string(8);
      ((size_t *)s)[0] = (size_t)extern_pos;
      ((size_t *)s)[1] = (size_t)res;
    }
  else
    {
      s = alloc_string(4 + extern_pos * sizeof(unsigned long));
      ((size_t *)s)[0] = (size_t)extern_pos;
      memmove(&Byte(s, 4), (char *)extern_block, extern_pos * sizeof(unsigned long));
    }
  stat_free((char *) extern_block);
  return s;
}

/* Turn an externalized ML value (a string) into an ML value, a la intern.c */

value mlval_string(value s)
{
  value res;
  mlsize_t whsize, wosize;
  unsigned long bhsize;
  color_t color;
  header_t hd;

  whsize = ((mlsize_t *)s)[0];

  if (whsize == 0) {
    res = (value) ((mlsize_t *)s)[1];
    if (IS_LONG(res))
      return res;
    else
      return Atom(res >> 2);
  }
  bhsize = Bsize_wsize (whsize);
  wosize = Wosize_whsize (whsize);

  if (wosize > Max_wosize)
    failwith("mlval_string: structure too big");
  res = alloc_shr(wosize, String_tag);
  hd = Hd_val (res);
  color = Color_hd (hd);
  assert (color == White || color == Black);
  if (bhsize + 4 > string_length(s)) {
    Hd_val (res) = hd;                      /* Avoid confusing the GC. */
    failwith ("mlval_string: truncated object");
  }
  memmove(Hp_val(res), &Byte(s, 4), bhsize);
  adjust_pointers((value*)(Hp_val (res)), whsize, color);

  return res;
}

/* Make a double from a float object, represented as a big-endian
   four-byte Word8Vector value */

value w8vectofloat(value v)
{
  /* The v vector must have length = 4 bytes */
  union { float flt; char w8[4]; } buf;
  int i;
  char* p = String_val(v);
  for (i=0; i<4; i++)
#ifdef WORDS_BIGENDIAN
    buf.w8[i] = p[i];
#else
    buf.w8[i] = p[3-i];
#endif

  return copy_double(buf.flt);
}

/* Make a big-endian four-byte Word8Vector value from a float,
   represented as a double. */

value floattow8vec(value v)
{
  union { float flt; char w8[4]; } buf;
  value res;
  char* p;
  int i;
  buf.flt = (float)(Double_val(v));
  res = alloc_string(4);
  p = String_val(res);
  for (i=0; i<4; i++)
#ifdef WORDS_BIGENDIAN
    p[i] = buf.w8[i];
#else
    p[i] = buf.w8[3-i];
#endif

  return res;
}

/* Make a double from a double object, represented as a big-endian
   eight-byte Word8Vector value */

value w8vectodouble(value v)
{
  /* The v vector must have length = 8 bytes */

  value res;

#ifdef WORDS_BIGENDIAN
  res = copy_double(Double_val(v));
#else
  PUSH_ROOTS(r, 1);
  r[0] = v;
  res = copy_double(0.0);
  {
    int i;
    for (i=0; i<8; i++)
      Byte(res, i) = Byte(r[0], 7-i);
  }
  POP_ROOTS();
#endif

  return res;
}

/* Make a big-endian eight-byte Word8Vector value from a double. */

value doubletow8vec(value v)
{
  value res;
  PUSH_ROOTS(r, 1);
  r[0] = v;
  res = alloc_string(8);
  Store_double_val(res, Double_val(r[0]));
  POP_ROOTS();

#ifndef WORDS_BIGENDIAN
  {
    int i;
    for (i=0; i<4; i++)
      {
	char tmp = Byte(res, i);
	Byte(res, i) = Byte(res, 7-i);
	Byte(res, 7-i) = tmp;
      }
  }
#endif

  return res;
}

/* Modified from John Reppy's code (see SML Basis mail of 1997-08-01) */

value sml_localoffset(value v)
{
  struct tm   *gmt;
  time_t      t1, t2;
  double      td;

  t1 = time((time_t*)0);
  gmt = gmtime (&t1);
  t2 = mktime(gmt);
  td = difftime(t2, t1);

  return copy_double(td); /* not SYStoSMLtime(td) */
}

/* Return a name (as a string) of SML exception exn */

value sml_exnname(value exn)
{
  value strval = Field(Field(exn, 0), 0);
  return strval;
}

/* Create a string representation of SML exception exn, if possible. */

char* exnmessage_aux(value exn)
{
#define BUFSIZE 256
  char* buf = (char*)malloc(BUFSIZE+1);
  /* An exn val is a pair (strref, argval) : string ref * 'a */
  value strref = Field(exn, 0);
  value strval = Field(strref, 0);
  value argval = Field(exn, 1);
  if (strref == Field(global_data, SYS__EXN_SYSERR)) {
    value msgval = Field(argval, 0);
    snprintf(buf, BUFSIZE, "%s: %s",
	     String_val(strval), String_val(msgval));
    return buf;
  } else if (strref == Field(global_data, SYS__EXN_IO)) {
    value causeval = Field(argval, 0);
    value fcnval   = Field(argval, 1);
    value nameval  = Field(argval, 2);
    char* causetxt = exnmessage_aux(causeval);
    snprintf(buf, BUFSIZE, "%s: %s failed on `%s'; %s",
	     String_val(strval), String_val(fcnval),
	     String_val(nameval), causetxt);
    free(causetxt);
    return buf;
  } else if (IS_BLOCK(argval)) {
    if (Tag_val(argval) == String_tag) {
      snprintf(buf, BUFSIZE, "%s: %s", String_val(strval), String_val(argval));
      return buf;
    } else if (Tag_val(argval) == Double_tag){
      char doubletxt[64];
      string_of_float_aux(doubletxt, Double_val(argval));
      snprintf(buf, BUFSIZE, "%s: %s", String_val(strval), doubletxt);
      return buf;
    }
  }
  /* If unknown exception, copy the name and return it */
  snprintf(buf, BUFSIZE, "%s", String_val(strval));
  return buf;
#undef BUFSIZE
}


/* Return a string representation of SML exception exn, if possible */

value sml_exnmessage(value exn)
{
  char* buf = exnmessage_aux(exn);
  value res = copy_string(buf);
  free(buf);
  return res;
}

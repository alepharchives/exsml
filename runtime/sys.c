/* Basic system calls */

#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "attributes.h"
#include "config.h"
#include "alloc.h"
#include "fail.h"
#include "global_tbl.h"
#include "instruct.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"
#include "io.h"

void sys_error();
void sys_exit(value);
value sys_open(value, value, value);
value sys_close(value);
value sys_remove(value);
value sys_rename(value, value);
value sys_chdir(value);
value sys_getenv(value);
value sys_system_command(value);
value mkexnname(char*);
value mkexn0val(value);
void intr_handler(int);
value sys_catch_break(value);
void float_handler(int);
static void init_float_handler(void);
void sys_init(char**);
char *searchpath(char*);

static void mysignal(int, void (*)(int));

char* globalexn[] = {
       "Out_of_memory",
       "Invalid_argument",
       "Graphic_failure",
       "SysErr",
       "Fail",
       "Size",
       "Interrupt",
       "Subscript",
       "Chr",
       "Div",
       "Domain",
       "Ord",
       "Overflow",
       "Bind",
       "Match",
       "Io" };


void sys_error()
{
  char *err = strerror(errno);
  value exnarg;

  /* Raise SysErr with argument (err, SOME errno) */

  PUSH_ROOTS(r, 2);
  r[0] = copy_string(err);	/* The error message string	*/

  r[1] = alloc(1, SOMEtag);	/* The SOME errno object	*/
  Field(r[1], 0) = LONG_TO_VAL(errno);

  exnarg = alloc_tuple(2);	/* The argument tuple		*/
  Field(exnarg, 0) = r[0];
  Field(exnarg, 1) = r[1];
  POP_ROOTS();

  raiseprimitive1(SYS__EXN_SYSERR, exnarg);
}

__attribute__((noreturn))
void sys_exit(value retcode)
{
  flush_stdouterr();
  exit(VAL_TO_INT(retcode));
}

static int sys_open_flags[] = {
  O_APPEND, 0, O_CREAT, O_EXCL, O_RDONLY, O_RDWR, 0, O_TRUNC, O_WRONLY
};

value sys_open(value path, value flags, value perm)
{
	int ret;
	ret = open(String_val(path), convert_flag_list(flags, sys_open_flags),
		   VAL_TO_INT(perm));
	if (ret == -1) sys_error(String_val(path));
	return LONG_TO_VAL(ret);
}

value sys_close(value fd)
{
  if (close(VAL_TO_INT(fd)) != 0) sys_error();
  return Atom(0);
}

value sys_remove(value name)
{
  int ret;
  ret = unlink(String_val(name));
  if (ret != 0) sys_error(String_val(name));
  return Atom(0);
}

value sys_rename(value oldname, value newname)
{
  if (rename(String_val(oldname), String_val(newname)) != 0)
    sys_error(String_val(oldname));
  return Atom(0);
}

value sys_chdir(value dirname)
{
  if (chdir(String_val(dirname)) != 0) sys_error(String_val(dirname));
  return Atom(0);
}

value sys_getenv(value var)
{
  char * res;

  res = getenv(String_val(var));
  if (res == 0) {
    raiseprimitive0(SYS__EXN_ARGUMENT);
  }
  return copy_string(res);
}

value sys_system_command(value command)
{
  int retcode = system(String_val(command));
  if (retcode == -1) sys_error(String_val(command));
  return INT_TO_VAL(retcode);
}

static int sys_var_init[] = {
  0400, 0200, 0100,
  0040, 0020, 0010,
  0004, 0002, 0001,
  04000, 02000,
  0444, 0222, 0111
};

/* Create an exn name = a string ref, from a null-terminated string,
   possibly in C malloc space */

value mkexnname(char* name) {
	value ref;

	PUSH_ROOTS(r, 1);
	r[0] = copy_string(name);
	ref = alloc_shr(1, Reference_tag);
	modify(&Field(ref, 0), r[0]);
	POP_ROOTS();

	return ref;
}

/* Create an exn value = a pair of a string ref and () : unit */

value mkexn0val(value exnname) {
  value exnval = alloc_tuple(2);
  modify(&Field(exnval, 0), Field(global_data, exnname));
  modify(&Field(exnval, 1), Val_unit);
  return exnval;
}

/* Handling of user interrupts and floating-point errors */

static void mysignal(int signum, void (*handler)(int)) {
	struct sigaction sigact;
	sigset_t emptyset;
	sigemptyset(&emptyset);
	sigact.sa_handler  = handler;
	sigact.sa_mask     = emptyset;
	sigact.sa_flags    = SA_NOMASK;
	sigaction(signum, &sigact, 0);
}

void intr_handler(int UNUSED(sig))
{
	mysignal (SIGINT, intr_handler);
	/* sigint_pending = 1; TODO: Where did this come from? */
}

value sys_catch_break(value onoff)
{
  if (Tag_val(onoff))
    mysignal(SIGINT, intr_handler);
  else
    mysignal(SIGINT, SIG_DFL);
  return Atom(0);
}

/* TODO: Perhaps change this one and intr_handler to something better */
void float_handler(int UNUSED(sig))
{
	mysignal (SIGFPE, float_handler);

	if (float_exn == Field(global_data, SYS__EXN_FAIL)) {
		failwith("floating point error");
	} else {
		raiseprimitive0(float_exn);
	}
}

static void init_float_handler(void)
{
  mysignal(SIGFPE, float_handler);
}

void sys_init(char ** argv)
{
	value v;
	int i;

	init_float_handler();

	v = copy_string_array(argv);
	modify(&Field(global_data, SYS__COMMAND_LINE), v);
	for (i = SYS__S_IRUSR; i <= SYS__S_IXALL; i++)
		Field(global_data, i) = LONG_TO_VAL(sys_var_init[i - SYS__S_IRUSR]);
	Field(global_data, SYS__INTERACTIVE) = Val_false;
	Field(global_data, SYS__MAX_VECT_LENGTH) = LONG_TO_VAL(Max_wosize);
	Field(global_data, SYS__MAX_STRING_LENGTH) =
		LONG_TO_VAL(Max_wosize * sizeof(value) - 2);

	/* Allocate the exn names for pervasize dynamic exceptions */
	for (i = SYS__FIRST_EXN; i <= SYS__LAST_EXN ; i++) {
		value exn = mkexnname(globalexn[i - SYS__FIRST_EXN]);
		modify(&Field(global_data, i), exn);
	}
	/* Allocate some exn values for use in interprete */
	modify(&Field(global_data, EXN_INTERRUPT), mkexn0val(SYS__EXN_INTERRUPT));
	modify(&Field(global_data, EXN_DIV),       mkexn0val(SYS__EXN_DIV));
	modify(&Field(global_data, EXN_OVERFLOW),  mkexn0val(SYS__EXN_OVERFLOW));
}


/* Search path function */
char * searchpath(char * name)
{
  static char fullname[512];
  char * path;
  char * p;
  char * q;

  for (p = name; *p != 0; p++) {
    if (*p == '/') return name;
  }
  path = getenv("PATH");
  if (path == 0) return 0;
  while(1) {
    p = fullname;
    while (*path != 0 && *path != ':') {
      *p++ = *path++;
    }
    if (p != fullname) *p++ = '/';
    q = name;
    while (*q != 0) {
      *p++ = *q++;
    }
    *p = 0;
    if (access(fullname, 1) == 0) return fullname;
    if (*path == 0) return 0;
    path++;
  }
}


#include "alloc.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"
#include "stacks.h"
#include "fail.h"
#include "instruct.h"
#include "interp.h"

volatile int signal_is_pending = 0;
int in_blocking_section = 0;
volatile bytecode_t signal_handler;
volatile int signal_number;

/* This is set by interprete in interp.c on initialization: */

bytecode_t raise_break_exn;

void execute_signal(void)
{
  if (in_blocking_section) {
    value clos;
    clos = alloc(Closure_wosize, Closure_tag);
    Code_val(clos) = signal_handler;
    Env_val(clos) = Atom(0);
    callback(clos, INT_TO_VAL(signal_number));
  } else {
    signal_is_pending = 1;
    something_to_do = 1;
  }
}

void enter_blocking_section(void)
{
  in_blocking_section = 1;
  if (signal_is_pending) execute_signal();
}

void leave_blocking_section(void)
{
  in_blocking_section = 0;
}

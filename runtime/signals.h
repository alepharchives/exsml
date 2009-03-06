#ifndef _signals_
#define _signals_

#include "misc.h"

extern volatile int signal_is_pending;
extern volatile CODE signal_handler;
extern volatile int signal_number;
extern int in_blocking_section;

void execute_signal (void);
extern void enter_blocking_section (void);
extern void leave_blocking_section (void);
extern CODE raise_break_exn;
#endif /* _signals_ */

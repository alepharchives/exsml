/* runtime.c  */

#include <sys/times.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

#include "config.h"
#include "runtime.h"

struct mosml_timeval gc_time = { (long) 0,
				 (long) 0 };

void beg_gc_time(void)
{
  struct rusage rusages;

  getrusage(RUSAGE_SELF, &rusages);

  gc_time.tv_sec  -= rusages.ru_utime.tv_sec;
  gc_time.tv_usec -= rusages.ru_utime.tv_usec;

  if (gc_time.tv_usec < 0) {
    gc_time.tv_usec += 1000000;
    gc_time.tv_sec  -= 1;
  }
}

void end_gc_time(void)
{
  struct rusage rusages;

  getrusage(RUSAGE_SELF, &rusages);

  gc_time.tv_sec  += rusages.ru_utime.tv_sec;
  gc_time.tv_usec += rusages.ru_utime.tv_usec;

  if (gc_time.tv_usec >= 1000000) {
    gc_time.tv_usec -= 1000000;
    gc_time.tv_sec  += 1;
  }
}

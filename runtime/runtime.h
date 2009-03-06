/* runtime.h */

void beg_gc_time();
void end_gc_time();

struct mosml_timeval {
	long	tv_sec;		/* seconds */
	long	tv_usec;	/* microseconds */
};

extern struct mosml_timeval gc_time;


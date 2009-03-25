/* Buffered input/output. */

#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>

#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "signals.h"
#include "sys.h"
#include "global_tbl.h"

/* Common functions. */

/* Improvement suggested by Doug Currie:
   memoize std_in, std_out and std_err.
   Used also in function flush_stdouterr.

   As usual, std_channel[0] = stdin,
             std_channel[1] = stdout,
	     std_channel[2] = stderr.
*/

static struct channel *std_channel[3] = {NULL, NULL, NULL};

value open_descriptor(value);
value channel_descriptor(struct channel *);
value channel_size(struct channel *);
value output_char(struct channel *, value);
value output_int(struct channel *, value);
value output(value, value, value, value);
void nonblocking_mode(int, int);
value close_out(struct channel *);
value input_char(struct channel *);
value input_int(struct channel *);
value input(value, value, value, value);
value input_nonblocking(value, value, value, value);
value seek_in(struct channel *, value);
value pos_in(struct channel *);
value input_scan_line(struct channel *);

struct channel * open_descr(int fd)
{
	struct channel * channel;

	if( (unsigned)fd < 3 && std_channel[fd] != NULL )
		return std_channel[fd];
	channel = (struct channel *) stat_alloc(sizeof(struct channel));
	channel->fd = fd;
	channel->offset = 0;
	channel->curr = channel->max = channel->buff;
	channel->end = channel->buff + IO_BUFFER_SIZE;
	if( (unsigned)fd < 3 )
		std_channel[fd] = channel;
	return channel;
}

value open_descriptor(value fd)
{
	return (value) open_descr(VAL_TO_INT(fd));
}

value channel_descriptor(struct channel * channel)
{
	return LONG_TO_VAL(channel->fd);
}

value channel_size(struct channel * channel)
{
	long end;

	end = lseek(channel->fd, 0, 2);
	if (end == -1) sys_error();
	if (lseek(channel->fd, channel->offset, 0) != channel->offset)
		sys_error();

	return LONG_TO_VAL(end);
}

/* Output */

static void really_write(int fd, char * p, int n)
{
	int retcode;
	while (n > 0) {
		/* I wonder if this can hang the shit */
		do { retcode = write(fd, p, n); }
		while (retcode == -1 && errno == EINTR);

		if (retcode == -1) sys_error();
		p += retcode;
		n -= retcode;
	}
}

value flush(struct channel * channel)
{
	int n;
	n = channel->max - channel->buff;
	if (n > 0) {
		really_write(channel->fd, channel->buff, n);
		channel->offset += n;
		channel->curr = channel->buff;
		channel->max  = channel->buff;
	}

	return Atom(0);
}

void flush_stdouterr(void)
{
	if (std_channel[1])
		flush(std_channel[1]);
	if (std_channel[2])
		flush(std_channel[2]);
}

value output_char(struct channel * channel, value ch)
{
	putch(channel, VAL_TO_LONG(ch));
	return Atom(0);
}

void putword(struct channel * channel, uint32_t w)
{
	putch(channel, w >> 24);
	putch(channel, w >> 16);
	putch(channel, w >> 8);
	putch(channel, w);
}

value output_int(struct channel * channel, value w)
{
	putword(channel, VAL_TO_LONG(w));
	return Atom(0);
}

void putblock(struct channel * channel, char * p, unsigned n)
{
	unsigned m;

	m = channel->end - channel->curr;
	if (channel->curr == channel->buff && n >= m) {
		really_write(channel->fd, p, n);
		channel->offset += n;
	} else if (n <= m) {
		memmove(channel->curr, p, n);
		channel->curr += n;
		if (channel->curr > channel->max) channel->max = channel->curr;
	} else {
		memmove(channel->curr, p, m);
		p += m;
		n -= m;
		m = channel->end - channel->buff;
		really_write(channel->fd, channel->buff, m);
		channel->offset += m;
		if (n <= m) {
			memmove(channel->buff, p, n);
			channel->curr = channel->max = channel->buff + n;
		} else {
			really_write(channel->fd, p, n);
			channel->offset += n;
			channel->curr = channel->max = channel->buff;
		}
	}
}

value output(value channel, value buff, value start, value length)
{
	putblock((struct channel *) channel,
		 &Byte(buff, VAL_TO_LONG(start)),
		 (unsigned) VAL_TO_LONG(length));
	return Atom(0);
}

value seek_out(struct channel * channel, value pos)
{
	long dest;

	dest = VAL_TO_LONG(pos);
	if (dest >= channel->offset &&
	    dest <= channel->offset + channel->max - channel->buff) {
		channel->curr = channel->buff + dest - channel->offset;
	} else {
		flush(channel);
		if (lseek(channel->fd, dest, 0) != dest) sys_error();
		channel->offset = dest;
	}
	return Atom(0);
}

value pos_out(struct channel * channel)
{
	return LONG_TO_VAL(channel->offset + channel->curr - channel->buff);
}

value close_out(struct channel * channel)
{
	if ((unsigned)(channel->fd) >= 3)
	{
		flush(channel);
		close(channel->fd);
		stat_free((char *) channel);
	}
	return Atom(0);
}

/* Input */
void nonblocking_mode(int fd, int nonblocking)
{
	int retcode = fcntl(fd, F_GETFL);
	if (retcode != -1) {
		if (nonblocking)
			retcode = fcntl(fd, F_SETFL, retcode | O_NONBLOCK);
		else
			retcode = fcntl(fd, F_SETFL, retcode & (~O_NONBLOCK));
	}

	if (retcode == -1)
		failwith("nonblocking_mode");
}

/* Risk: If an interrupt occurs after non-blocking mode has been set,
   and before it has been reset (unlikely, just because the read is
   non-blocking), then the next read on the same fd will be
   non-blocking, whether or not that was the intention.  This may
   cause SysError "Resource temporarily unavailable" to be raised,
   when one would have expected the read to block.  Unlikely to be a
   problem in practice, since reads on an fd are likely to be all
   blocking or all non-blocking.  sestoft 2000-03-15 */

static int really_read(int fd, char * p, unsigned n, int nonblocking)
{
	int retcode;

	if (nonblocking) {
		nonblocking_mode(fd, nonblocking);	   /* set non-blocking */
	}

	enter_blocking_section();

	do {
		retcode = read(fd, p, n);
	} while (retcode == -1 && errno == EINTR);

	leave_blocking_section();

	if (nonblocking) {
		nonblocking_mode(fd, 0);			/* unset non-blocking */
		if (retcode == -1 && errno != EAGAIN)
			sys_error();
	} else if (retcode == -1) {
		sys_error();
	}

	return retcode;
}

unsigned char refill(struct channel * channel)
{
	int n;

	n = really_read(channel->fd, channel->buff, IO_BUFFER_SIZE,
			/* nonblocking = */ 0);
	if (n == 0) raiseprimitive0(SYS__EXN_SIZE);
	channel->offset += n;
	channel->max = channel->buff + n;
	channel->curr = channel->buff + 1;
	return (unsigned char)(channel->buff[0]);
}

value input_char(struct channel * channel)
{
	unsigned char c;
	c = getch(channel);
	return LONG_TO_VAL(c);
}

uint32_t getword(struct channel * channel)
{
	int i;
	uint32_t res;

	res = 0;
	for(i = 0; i < 4; i++) {
		res = (res << 8) + getch(channel);
	}
	return res;
}

value input_int(struct channel * channel)
{
	long i;
	i = getword(channel);
#if (SIZEOF_LONG == 8)
	i = (i << 32) >> 32;          /* Force sign extension */
#endif
	return LONG_TO_VAL(i);
}

int getblock(struct channel * channel, char * p, size_t n, int nonblocking)
{
	int dread;
	unsigned m;

	m = channel->max - channel->curr;
	if (n <= m) {
		memmove(p, channel->curr, n);
		channel->curr += n;
		return n;
	} else if (m > 0) {
		memmove(p, channel->curr, m);
		channel->curr += m;
		return m;
	} else if (n < IO_BUFFER_SIZE) {
		dread = really_read(channel->fd, channel->buff, IO_BUFFER_SIZE, nonblocking);
		if (dread == -1) {/* Non-blocking read returned no data */
			return -1;
		} else {
			assert(dread >= 0);
			channel->offset += dread;
			channel->max = channel->buff + dread;
			if (n > ((unsigned) dread)) {
				n = dread;
			}
			memmove(p, channel->buff, n);
			channel->curr = channel->buff + n;
			return n;
		}
	} else {
		channel->curr = channel->buff;
		channel->max = channel->buff;
		dread = really_read(channel->fd, p, n, nonblocking);
		if (dread == -1)	/* Non-blocking read returned no data */
			return -1;
		else {
			channel->offset += dread;
			return dread;
		}
	}
}

int really_getblock(struct channel * chan, char * p, unsigned long n)
{
	unsigned r;
	while (n > 0) {
		r = (unsigned)getblock(chan, p, (unsigned) n,  /* nonblocking = */ 0);
		if (r == 0) return 0;
		p += r;
		n -= r;
	}
	return 1;
}



value input(value channel, value buff, value start, value length)
{
	return LONG_TO_VAL(getblock((struct channel *) channel,
				 &Byte(buff, VAL_TO_LONG(start)),
				 (unsigned) VAL_TO_LONG(length),
				 /* nonblocking = */ 0));
}

value input_nonblocking(value channel, value buff, value start, value length)
{
	int n = getblock((struct channel *) channel,
			 &Byte(buff, VAL_TO_LONG(start)),
			 (unsigned) VAL_TO_LONG(length),
			 /* nonblocking = */ 1);
	if (n == -1)		/* Non-blocking read returned no data */
		return NONE;
	else {
		value res = alloc(1, SOMEtag);
		Field(res, 0) = LONG_TO_VAL(n);
		return res;
	}
}

value seek_in(struct channel * channel, value pos)
{
	long dest;

	dest = VAL_TO_LONG(pos);
	if (dest >= channel->offset - (channel->max - channel->buff) &&
	    dest <= channel->offset) {
		channel->curr = channel->max - (channel->offset - dest);
	} else {
		if (lseek(channel->fd, dest, 0) != dest) sys_error();
		channel->offset = dest;
		channel->curr = channel->max = channel->buff;
	}

	return Atom(0);
}

value pos_in(struct channel * channel)
{
	return LONG_TO_VAL(channel->offset - (channel->max - channel->curr));
}

value close_in(struct channel * channel)
{
	close(channel->fd);
	stat_free((char *) channel);
	return Atom(0);
}

value input_scan_line(struct channel * channel)
{
	char * p;
	int n;

	p = channel->curr;
	do {
		if (p >= channel->max) {
			/* No more characters available in the buffer */
			if (channel->curr > channel->buff) {
				/* Try to make some room in the buffer by shifting the unread
				   portion at the beginning */
				memmove(channel->buff, channel->curr, channel->max - channel->curr);
				n = channel->curr - channel->buff;
				channel->curr -= n;
				channel->max -= n;
				p -= n;
			}
			if (channel->max >= channel->end) {
				/* Buffer is full, no room to read more characters from the input.
				   Return the number of characters in the buffer, with negative
				   sign to indicate that no newline was encountered. */
				return LONG_TO_VAL(-(channel->max - channel->curr));
			}
			/* Fill the buffer as much as possible */
			n = really_read(channel->fd, channel->max, channel->end - channel->max,
					/* nonblocking = */ 0);
			if (n == 0) {
				/* End-of-file encountered. Return the number of characters in the
				   buffer, with negative sign since we haven't encountered
				   a newline. */
				return LONG_TO_VAL(-(channel->max - channel->curr));
			}
			channel->offset += n;
			channel->max += n;
		}
	} while (*p++ != '\n');
	/* Found a newline. Return the length of the line, newline included. */
	return LONG_TO_VAL(p - channel->curr);
}

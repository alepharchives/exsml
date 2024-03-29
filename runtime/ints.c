#include <stdio.h>
#include <stdlib.h>

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"

value int_of_string(value);
value format_int(value, value);

value int_of_string(value s)
{
	long res;
	int sign;
	int base;
	char * p;
	int c, d;

	p = String_val(s);
	if (*p == 0) {
		failwith("int_of_string");
	}

	sign = 1;
	if (*p == '-') {
		sign = -1;
		p++;
	}
	base = 10;
	if (*p == '0') {
		switch (p[1]) {
		case 'x': case 'X':
			base = 16; p += 2; break;
		case 'o': case 'O':
			base = 8; p += 2; break;
		case 'b': case 'B':
			base = 2; p += 2; break;
		default:
			perror("Internal error in int_of_string");
		}
	}
	res = 0;
	while (1) {
		c = *p;
		if (c >= '0' && c <= '9')
			d = c - '0';
		else if (c >= 'A' && c <= 'F')
			d = c - 'A' + 10;
		else if (c >= 'a' && c <= 'f')
			d = c - 'a' + 10;
		else
			break;
		if (d >= base) break;
		res = base * res + d;
		p++;
	}
	if (*p != 0)
		failwith("int_of_string");
	return LONG_TO_VAL(sign < 0 ? -res : res);
}


value format_int(value fmt, value arg)
{
	char format_buffer[32];
	size_t prec;
	char * p;
	char * dest;
	value res;

	prec = 32;
	for (p = String_val(fmt); *p != 0; p++) {
		if (*p >= '0' && *p <= '9') {
			prec = strtoul(p, NULL, 10);
			break;
		}
	}

	if (prec <= sizeof(format_buffer)) {
		dest = format_buffer;
	} else {
		dest = stat_alloc(prec);
	}
	sprintf(dest, String_val(fmt), VAL_TO_LONG(arg));
	res = copy_string(dest);
	if (dest != format_buffer) {
		stat_free(dest);
	}

	return res;
}

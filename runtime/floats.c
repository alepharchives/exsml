#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "alloc.h"
#include "debugger.h"
#include "fail.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"

value format_float(value, value);
value float_of_string(value);
value exp_float(value);
value log_float(value);
value sqrt_float(value);
value power_float(value, value);
value sin_float(value);
value cos_float(value);
value tan_float(value);
value asin_float(value);
value acos_float(value);
value atan_float(value);
value atan2_float(value, value);

#ifdef ALIGN_DOUBLE

double Double_val(value val)
{
	union { value v[2]; double d; } buffer;

	assert(sizeof(double) == 2 * sizeof(value));

	buffer.v[0] = Field(val, 0);
	buffer.v[1] = Field(val, 1);
	return buffer.d;
}

void Store_double_val(value val, double dbl)
{
	union { value v[2]; double d; } buffer;

	assert(sizeof(double) == 2 * sizeof(value));
	if (dbl == -0) {
		dbl = 0;
	}

	buffer.d = dbl;
	Field(val, 0) = buffer.v[0];
	Field(val, 1) = buffer.v[1];
}

#else

void Store_double_val(value val, double dbl)
{
	if (dbl == -0) {
		* (double *) (val) = 0;
	} else {
		* (double *) (val) = dbl;
	}
}
#endif


value format_float(value fmt, value arg)
{
	char format_buffer[64];
	size_t prec, i;
	char * p;
	char * dest;
	value res;

	prec = 64;
	for (p = String_val(fmt); *p != 0; p++) {
		if (*p >= '0' && *p <= '9') {
			i = atoi(p) + 15;
			if (i > prec) prec = i;
			break;
		}
	}

	for( ; *p != 0; p++) {
		if (*p == '.') {
			i = atoi(p+1) + 15;
			if (i > prec) prec = i;
			break;
		}
	}

	if (prec <= sizeof(format_buffer)) {
		dest = format_buffer;
	} else {
		dest = stat_alloc(prec);
	}

	sprintf(dest, String_val(fmt), Double_val(arg));
	res = copy_string(dest);
	if (dest != format_buffer) {
		stat_free(dest);
	}

	return res;
}

value float_of_string(value s)
{
	return copy_double(atof(String_val(s)));
}

value exp_float(value f)
{
	return copy_double(exp(Double_val(f)));
}

value log_float(value f)
{
	return copy_double(log(Double_val(f)));
}

value sqrt_float(value f)
{
	return copy_double(sqrt(Double_val(f)));
}

value power_float(value f, value g)
{
	return copy_double(pow(Double_val(f), Double_val(g)));
}

value sin_float(value f)
{
	return copy_double(sin(Double_val(f)));
}

value cos_float(value f)
{
	return copy_double(cos(Double_val(f)));
}

value tan_float(value f)
{
	return copy_double(tan(Double_val(f)));
}

value asin_float(value f)
{
	return copy_double(asin(Double_val(f)));
}

value acos_float(value f)
{
	return copy_double(acos(Double_val(f)));
}

value atan_float(value f)
{
	return copy_double(atan(Double_val(f)));
}

value atan2_float(value f, value g)
{
	return copy_double(atan2(Double_val(f), Double_val(g)));
}


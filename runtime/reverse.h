/* Swap byte-order in 16-bit, 32-bit and 64-bit words */

#ifndef _reverse_
#define _reverse_

#include "config.h"

#define Reverse_short(s) {                                                    \
  char * _p;                                                                  \
  int _a;                                                                     \
  _p = (char *) (s);                                                          \
  _a = _p[0];                                                                 \
  _p[0] = _p[1];                                                              \
  _p[1] = _a;                                                                 \
}

#define Reverse_int32_t(w) {                                                  \
  char * _p;                                                                  \
  int _a;                                                                     \
  _p = (char *) (w);                                                          \
  _a = _p[0];                                                                 \
  _p[0] = _p[3];                                                              \
  _p[3] = _a;                                                                 \
  _a = _p[1];                                                                 \
  _p[1] = _p[2];                                                              \
  _p[2] = _a;                                                                 \
}

#define Reverse_int64_t(d) {                                                  \
  char * _p;                                                                  \
  int _a;                                                                     \
  _p = (char *) (d);                                                          \
  _a = _p[0];                                                                 \
  _p[0] = _p[7];                                                              \
  _p[7] = _a;                                                                 \
  _a = _p[1];                                                                 \
  _p[1] = _p[6];                                                              \
  _p[6] = _a;                                                                 \
  _a = _p[2];                                                                 \
  _p[2] = _p[5];                                                              \
  _p[5] = _a;                                                                 \
  _a = _p[3];                                                                 \
  _p[3] = _p[4];                                                              \
  _p[4] = _a;                                                                 \
}

#define Reverse_double Reverse_int64_t

#if (SIZEOF_LONG_P == 8)
#define Reverse_word Reverse_int64_t
#elif (SIZEOF_LONG_P == 4)
#define Reverse_word Reverse_int32_t
#else
#error "Unknown architecture size"
#endif

#endif /* _reverse_ */

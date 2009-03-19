/*
 * The mosml-config.h file uses the autotools generated
 * configuration file to set preprocessor macros correctly
 * for the rest of the source code
 *
 */

#ifndef MOSML_CONFIG_H
#define MOSML_CONFIG_H

#include "config.h"

#if (SIZEOF_LONG_P == 8)
#define SIXTYFOUR
#define Reverse_word Reverse_int64
#define PC_FORMAT "%6ld  "
#elif (SIZEOF_LONG_P == 4)
#define THIRTYTWO
#define Reverse_word Reverse_int32_t
#define PC_FORMAT "%6d   "
#else
#error "Unknown architecture size"
#endif


#endif

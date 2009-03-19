/*
 * The mosml-config.h file uses the autotools generated
 * configuration file to set preprocessor macros correctly
 * for the rest of the source code
 *
 */

#ifndef MOSML_CONFIG_H
#define MOSML_CONFIG_H

#include "config.h"

#if ((SIZEOF_INT == 4) && (SIZEOF_LONG == 8) && (SIZEOF_LONG_P == 8))
#define SIXTYFOUR
#elif  ((SIZEOF_INT == 4) && (SIZEOF_LONG == 4) && (SIZEOF_LONG_P == 4))
#define THIRTYTWO
#else
#error "Unknown architecture size"
#endif


#endif

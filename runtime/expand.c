/* Expanding the bytecode to threaded code */
/* sestoft@dina.kvl.dk 1999-01-21 */

#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>

#include "fail.h"
#include "instruct.h"
#include "misc.h"
#include "unalignd.h"
#include "interp.h"

/* Accessing arguments in the bytecode array: */

#define s16pc s16(pc)
#define u16pc u16(pc)
#define s32pc s32(pc)
#define u32pc u32(pc)
#define SHORT  (sizeof(short))
#define LONG   (sizeof(int32_t))

/* Computing the length of the realcode array, and building the
   address offset translation.

   In the bytecode, a label is represented by a signed 32-bit offset
   from the current bytecode PC; hence the jump is done by
	pcb = pcb + *pcb

   In the threaded code we want to replace this by the relevant index
   into the realcode array, thus avoiding the addition when doing the
   jump.  At runtime the jump should be done by:
        pcr = *pcr

   When preparing the threaded code we therefore need to know the
   realcode[] index corresponding to the bytecode index pcb + *pcb.
   One way to compute this is to maintain a mapping from all bytecode
   addresses to the corresponding realcode addresses.

   This can be built incrementally while computing the length of the
   realcode corresponding to the bytecode.

   Perhaps this scheme can be optimized, but let's leave that for
   later.
*/

/* Computes and returns the length of the required realcode array;
   allocates and fills in the realaddress array; the argument
   realaddress is a pointer to a variable pointing to that array.  */

int buildrealmap(bytecode_t byteprog, int code_size, int realaddress[])
{
  return 0;			/* bogus */
}

/* Computing the real address from the bytecode address and an offset */

#define REALADDR(pc, offset) \
  realprog + realaddress[(int)(pc - byteprog) + (int)offset];

/* The expander itself */
/* Returns a pointer to the realcode array (an array of C code addresses). */

realcode_t expandcode(bytecode_t byteprog, int code_size, void * jumptable[])
{
  return NULL;
} // expandcode

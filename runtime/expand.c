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
#ifdef THREADED
  int realsize = 0;
  bytecode_t pc = byteprog;

  /* Initialize to catch errors */
  int i;
  for (i=0; i<code_size; i++)
    realaddress[i] = -1;

  while (pc-byteprog < code_size) {
    int cur_inst = *pc;
    //    printf("%d:%d\t real = %d\n", pc-byteprog, cur_inst, realsize);
    realaddress[pc++ - byteprog] = realsize++;

    switch (cur_inst) {

    /* No arguments: */
    case SWAP:
    case PUSH:
    case PUSHACC0:
    case ACC0:
    case PUSHACC1:
    case ACC1:
    case PUSHACC2:
    case ACC2:
    case PUSHACC3:
    case ACC3:
    case PUSHACC4:
    case ACC4:
    case PUSHACC5:
    case ACC5:
    case PUSHACC6:
    case ACC6:
    case PUSHACC7:
    case ACC7:
    case PUSHENV1:
    case ENV1:
    case PUSHENV2:
    case ENV2:
    case PUSHENV3:
    case ENV3:
    case PUSHENV4:
    case ENV4:
    case PUSHENV5:
    case ENV5:
    case PUSHENV6:
    case ENV6:
    case PUSHENV7:
    case ENV7:
    case PUSH_ENV1_APPLY1:
    case PUSH_ENV1_APPLY2:
    case PUSH_ENV1_APPLY3:
    case PUSH_ENV1_APPLY4:
    case APPLY1:
    case APPLY2:
    case APPLY3:
    case APPLY4:
    case RETURN1:
    case RETURN2:
    case RESTART:
    case UPDATE:
    case CHECK_SIGNALS:
    case PUSHATOM0:
    case ATOM0:
    case ATOM1:
    case ATOM2:
    case ATOM3:
    case ATOM4:
    case ATOM5:
    case ATOM6:
    case ATOM7:
    case ATOM8:
    case ATOM9:
    case GETFIELD0:
    case GETFIELD1:
    case GETFIELD2:
    case GETFIELD3:
    case GETFIELD0_0:
    case GETFIELD0_1:
    case GETFIELD1_0:
    case GETFIELD1_1:
    case SETFIELD0:
    case SETFIELD1:
    case SETFIELD2:
    case SETFIELD3:
    case VECTLENGTH:
    case GETVECTITEM:
    case SETVECTITEM:
    case GETSTRINGCHAR:
    case SETSTRINGCHAR:
    case BOOLNOT:
    case POPTRAP:
    case RAISE:
    case PUSHCONST0:
    case CONST0:
    case PUSHCONST1:
    case CONST1:
    case PUSHCONST2:
    case CONST2:
    case PUSHCONST3:
    case CONST3:
    case ADDINT:
    case SUBINT:
    case MULINT:
    case DIVINT:
    case MODINT:
    case ANDINT:
    case ORINT:
    case XORINT:
    case SHIFTLEFTINT:
    case SHIFTRIGHTINTSIGNED:
    case SHIFTRIGHTINTUNSIGNED:
    case TAGOF:
    case EQ:
    case NEQ:
    case LTINT:
    case GTINT:
    case LEINT:
    case GEINT:
    case EQUNSIGN:
    case NEQUNSIGN:
    case LTUNSIGN:
    case GTUNSIGN:
    case LEUNSIGN:
    case GEUNSIGN:
    case FLOATOFINT:
    case SMLNEGFLOAT:
    case SMLADDFLOAT:
    case SMLSUBFLOAT:
    case SMLMULFLOAT:
    case SMLDIVFLOAT:
    case INTOFFLOAT:
    case EQFLOAT:
    case NEQFLOAT:
    case LTFLOAT:
    case GTFLOAT:
    case LEFLOAT:
    case GEFLOAT:
    case STRINGLENGTH:
    case EQSTRING:
    case NEQSTRING:
    case LTSTRING:
    case GTSTRING:
    case LESTRING:
    case GESTRING:
    case MAKEVECTOR:
    case SMLNEGINT:
    case SMLSUCCINT:
    case SMLPREDINT:
    case SMLADDINT:
    case SMLSUBINT:
    case SMLMULINT:
    case SMLDIVINT:
    case SMLMODINT:
    case MAKEREFVECTOR:
    case SMLQUOTINT:
    case SMLREMINT:
    case STOP:
      break;

    /* A one-byte argument: */
    case APPLY:
    case GRAB:
    case PUSHATOM:
    case ATOM:
    case MAKEBLOCK1:
    case MAKEBLOCK2:
    case MAKEBLOCK3:
    case MAKEBLOCK4:
    case CONSTBYTE:
      pc++; realsize++;
      break;

    /* A four-byte label argument.  The label should be translated to
       an address in the realprog[] array.  This requires an auxiliary
       table.  */
    case PUSH_RETADDR:
    case PUSHTRAP:
    case BRANCH:
    case BRANCHIF:
    case BRANCHIFNOT:
    case POPBRANCHIFNOT:
    case BRANCHIFEQ:
    case BRANCHIFNEQ:
    case BRANCHIFLT:
    case BRANCHIFGT:
    case BRANCHIFLE:
    case BRANCHIFGE:
      pc += LONG; realsize++;
      break;

    /* Two four-byte label arguments.  The labels should
       be translated to an address in the realprog[] array.  This
       requires an auxiliary table.
    */
    case BRANCHINTERVAL:
      pc += LONG; realsize++;
      pc += LONG; realsize++;
      break;

    /* A two-byte signed argument. */
    case CONSTSHORT:
      pc += SHORT; realsize++;
      break;

    /* A two-byte unsigned argument. */
    case PUSHACC:
    case ACCESS:
    case POP:
    case ASSIGN:
    case PUSHENVACC:
    case ENVACC:
    case DUMMY:
    case RETURN:
    case SETGLOBAL:
    case GETGLOBAL:
    case APPTERM1:
    case APPTERM2:
    case APPTERM3:
    case APPTERM4:
    case PUSH_ENV1_APPTERM1:
    case PUSH_ENV1_APPTERM2:
    case PUSH_ENV1_APPTERM3:
    case PUSH_ENV1_APPTERM4:
    case PUSH_GETGLOBAL:
    case PUSH_GETGLOBAL_APPLY1:
    case PUSH_GETGLOBAL_APPLY2:
    case PUSH_GETGLOBAL_APPLY3:
    case PUSH_GETGLOBAL_APPLY4:
    case GETFIELD:
    case SETFIELD:
    case C_CALL1:
    case C_CALL2:
    case C_CALL3:
    case C_CALL4:
    case C_CALL5:
      pc += SHORT; realsize++;
      break;

    /* A four-byte unsigned argument. */
    case MAKEBLOCK:
      pc += LONG; realsize++;
      break;

    /* A four-byte signed argument. */
    case PUSHCONSTINT:
    case CONSTINT:
      pc += LONG; realsize++;
      break;

    /* A one-byte argument and a four-byte signed (label) argument. */
    case CLOSURE:
    case CLOSREC:
    case BRANCHIFNEQTAG:
      pc++; realsize++;
      pc += LONG; realsize++;
      break;

    /* A one-byte argument and a two-byte unsigned argument. */
    case APPTERM:
    case C_CALLN:
      pc++; realsize++;
      pc += SHORT; realsize++;
      break;

    /* Two two-byte unsigned arguments. */
    case PUSH_GETGLOBAL_APPTERM1:
    case PUSH_GETGLOBAL_APPTERM2:
    case PUSH_GETGLOBAL_APPTERM3:
    case PUSH_GETGLOBAL_APPTERM4:
      pc += SHORT; realsize++;
      pc += SHORT; realsize++;
      break;

    /* A one-byte argument and a table of four-byte signed (label) arguments */
    /* We keep the byte argument for consistency.                           */
    case SWITCH:
      {
	int n = *pc;
	pc++; realsize++;		/* The byte     */
	pc += n * LONG; realsize += n;	/* The n labels */
      }
      break;
    default:
      printf("buildrealmap: opcode = %d at %ld\n", *pc, pc-byteprog);
      fatal_error("bad opcode\n");
    }
  }
  //  printf("buildrealmap finished\n");
  return realsize;
#else
  return 0;			/* bogus */
#endif
} // buildrealmap

/* Computing the real address from the bytecode address and an offset */

#define REALADDR(pc, offset) \
  realprog + realaddress[(int)(pc - byteprog) + (int)offset];

/* The expander itself */
/* Returns a pointer to the realcode array (an array of C code addresses). */

realcode_t expandcode(bytecode_t byteprog, int code_size, void * jumptable[])
{
#ifdef THREADED
  bytecode_t pc = byteprog;
  int *realaddress = (int*)(malloc(code_size * sizeof(int)));
  int realsize = buildrealmap(byteprog, code_size, realaddress);
  realcode_t realprog = (realcode_t)malloc(realsize * sizeof(void*));
  int codeptr = 0;

  while (pc - byteprog < code_size) {
    //    printf("%d:%d\t\n", pc-byteprog, *pc);
    switch (*pc) {

    /* No arguments: */
    case SWAP:
    case PUSH:
    case PUSHACC0:
    case ACC0:
    case PUSHACC1:
    case ACC1:
    case PUSHACC2:
    case ACC2:
    case PUSHACC3:
    case ACC3:
    case PUSHACC4:
    case ACC4:
    case PUSHACC5:
    case ACC5:
    case PUSHACC6:
    case ACC6:
    case PUSHACC7:
    case ACC7:
    case PUSHENV1:
    case ENV1:
    case PUSHENV2:
    case ENV2:
    case PUSHENV3:
    case ENV3:
    case PUSHENV4:
    case ENV4:
    case PUSHENV5:
    case ENV5:
    case PUSHENV6:
    case ENV6:
    case PUSHENV7:
    case ENV7:
    case PUSH_ENV1_APPLY1:
    case PUSH_ENV1_APPLY2:
    case PUSH_ENV1_APPLY3:
    case PUSH_ENV1_APPLY4:
    case APPLY1:
    case APPLY2:
    case APPLY3:
    case APPLY4:
    case RETURN1:
    case RETURN2:
    case RESTART:
    case UPDATE:
    case CHECK_SIGNALS:
    case PUSHATOM0:
    case ATOM0:
    case ATOM1:
    case ATOM2:
    case ATOM3:
    case ATOM4:
    case ATOM5:
    case ATOM6:
    case ATOM7:
    case ATOM8:
    case ATOM9:
    case GETFIELD0:
    case GETFIELD1:
    case GETFIELD2:
    case GETFIELD3:
    case GETFIELD0_0:
    case GETFIELD0_1:
    case GETFIELD1_0:
    case GETFIELD1_1:
    case SETFIELD0:
    case SETFIELD1:
    case SETFIELD2:
    case SETFIELD3:
    case VECTLENGTH:
    case GETVECTITEM:
    case SETVECTITEM:
    case GETSTRINGCHAR:
    case SETSTRINGCHAR:
    case BOOLNOT:
    case POPTRAP:
    case RAISE:
    case PUSHCONST0:
    case CONST0:
    case PUSHCONST1:
    case CONST1:
    case PUSHCONST2:
    case CONST2:
    case PUSHCONST3:
    case CONST3:
    case ADDINT:
    case SUBINT:
    case MULINT:
    case DIVINT:
    case MODINT:
    case ANDINT:
    case ORINT:
    case XORINT:
    case SHIFTLEFTINT:
    case SHIFTRIGHTINTSIGNED:
    case SHIFTRIGHTINTUNSIGNED:
    case TAGOF:
    case EQ:
    case NEQ:
    case LTINT:
    case GTINT:
    case LEINT:
    case GEINT:
    case EQUNSIGN:
    case NEQUNSIGN:
    case LTUNSIGN:
    case GTUNSIGN:
    case LEUNSIGN:
    case GEUNSIGN:
    case FLOATOFINT:
    case SMLNEGFLOAT:
    case SMLADDFLOAT:
    case SMLSUBFLOAT:
    case SMLMULFLOAT:
    case SMLDIVFLOAT:
    case INTOFFLOAT:
    case EQFLOAT:
    case NEQFLOAT:
    case LTFLOAT:
    case GTFLOAT:
    case LEFLOAT:
    case GEFLOAT:
    case STRINGLENGTH:
    case EQSTRING:
    case NEQSTRING:
    case LTSTRING:
    case GTSTRING:
    case LESTRING:
    case GESTRING:
    case MAKEVECTOR:
    case SMLNEGINT:
    case SMLSUCCINT:
    case SMLPREDINT:
    case SMLADDINT:
    case SMLSUBINT:
    case SMLMULINT:
    case SMLDIVINT:
    case SMLMODINT:
    case MAKEREFVECTOR:
    case SMLQUOTINT:
    case SMLREMINT:
    case STOP:
      realprog[codeptr++] = jumptable[*pc++];
      break;

    /* A one-byte argument: */
    case APPLY:
    case GRAB:
    case PUSHATOM:
    case ATOM:
    case MAKEBLOCK1:
    case MAKEBLOCK2:
    case MAKEBLOCK3:
    case MAKEBLOCK4:
    case CONSTBYTE:
      realprog[codeptr++] = jumptable[*pc++];
      realprog[codeptr++] = (void*)(long)(*pc++);
      break;

    /* A four-byte label argument.  The label is translated to an index
       into the realprog[] array. */
    case PUSH_RETADDR:
    case PUSHTRAP:
    case BRANCH:
    case BRANCHIF:
    case BRANCHIFNOT:
    case POPBRANCHIFNOT:
    case BRANCHIFEQ:
    case BRANCHIFNEQ:
    case BRANCHIFLT:
    case BRANCHIFGT:
    case BRANCHIFLE:
    case BRANCHIFGE:
      realprog[codeptr++] = jumptable[*pc++];
      realprog[codeptr++] = REALADDR(pc, s32pc); pc += LONG;
      break;

    /* Two four-byte label arguments.  The labels should
       be translated to an address in the realprog[] array.  This
       requires an auxiliary table.
    */
    case BRANCHINTERVAL:
      realprog[codeptr++] = jumptable[*pc++];
      realprog[codeptr++] = REALADDR(pc, s32pc);
      pc += LONG;
      realprog[codeptr++] = REALADDR(pc, s32pc);
      pc += LONG;
      break;

    /* A two-byte signed argument. */
    case CONSTSHORT:
      realprog[codeptr++] = jumptable[*pc++];
      realprog[codeptr++] = (void*)(long)s16pc;
      pc += SHORT;
      break;

    /* A two-byte unsigned argument. */
    case PUSHACC:
    case ACCESS:
    case POP:
    case ASSIGN:
    case PUSHENVACC:
    case ENVACC:
    case DUMMY:
    case RETURN:
    case SETGLOBAL:
    case GETGLOBAL:
    case APPTERM1:
    case APPTERM2:
    case APPTERM3:
    case APPTERM4:
    case PUSH_ENV1_APPTERM1:
    case PUSH_ENV1_APPTERM2:
    case PUSH_ENV1_APPTERM3:
    case PUSH_ENV1_APPTERM4:
    case PUSH_GETGLOBAL:
    case PUSH_GETGLOBAL_APPLY1:
    case PUSH_GETGLOBAL_APPLY2:
    case PUSH_GETGLOBAL_APPLY3:
    case PUSH_GETGLOBAL_APPLY4:
    case GETFIELD:
    case SETFIELD:
    case C_CALL1:
    case C_CALL2:
    case C_CALL3:
    case C_CALL4:
    case C_CALL5:
      realprog[codeptr++] = jumptable[*pc++];
      realprog[codeptr++] = (void*)(unsigned long)u16pc;
      pc += SHORT;
      break;

    /* A four-byte unsigned argument. */
    case MAKEBLOCK:
      realprog[codeptr++] = jumptable[*pc++];
      realprog[codeptr++] = (void*)(unsigned long)u32pc;
      pc += LONG;
      break;

    /* A four-byte signed argument. */
    case PUSHCONSTINT:
    case CONSTINT:
      realprog[codeptr++] = jumptable[*pc++];
      realprog[codeptr++] = (void*)(long)s32pc;
      pc += LONG;
      break;

    /* A one-byte argument and a four-byte signed (label) argument. */
    case CLOSURE:
    case CLOSREC:
    case BRANCHIFNEQTAG:
      realprog[codeptr++] = jumptable[*pc++];
      realprog[codeptr++] = (void*)(unsigned long)*pc++;
      realprog[codeptr++] = REALADDR(pc, s32pc);
      pc += LONG;
      break;

    /* A one-byte argument and a two-byte unsigned argument. */
    case APPTERM:
    case C_CALLN:
      realprog[codeptr++] = jumptable[*pc++];
      realprog[codeptr++] = (void*)(long)*pc++;
      realprog[codeptr++] = (void*)(unsigned long)u16pc;
      pc += SHORT;
      break;

    /* Two two-byte unsigned arguments. */
    case PUSH_GETGLOBAL_APPTERM1:
    case PUSH_GETGLOBAL_APPTERM2:
    case PUSH_GETGLOBAL_APPTERM3:
    case PUSH_GETGLOBAL_APPTERM4:
      realprog[codeptr++] = jumptable[*pc++];
      realprog[codeptr++] = (void*)(unsigned long)u16pc; pc += SHORT;
      realprog[codeptr++] = (void*)(unsigned long)u16pc; pc += SHORT;
      break;

    /* A one-byte argument and a table of four-byte signed (label) arguments. */
    /* We keep the byte argument for consistency.                            */
    case SWITCH:
      {
	unsigned long i, n;
	bytecode_t pc1;
	realprog[codeptr++] = jumptable[*pc++];
	n = (unsigned long)*pc++;
	realprog[codeptr++] = (void*)n;
	pc1 = pc;
	for (i=0; i<n; i++) {
	  realprog[codeptr++] = REALADDR(pc1, s32pc);
	  pc += LONG;
	}
      }
      break;
    default:
      printf("expandcode: opcode = %d at %ld\n", *pc, pc-byteprog);
      fatal_error("bad opcode");
    }
  }
  //  printf("expandcode finished\n");
  free(realaddress);
  //  printf("freed realaddress\n");
  return realprog;
#else
  return NULL;
#endif
} // expandcode

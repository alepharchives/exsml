#include "mlvalues.h"

extern value interprete(int mode, bytecode_t bprog, int code_size, CODE* rprog);
extern value callback(value closure, value arg);
extern value callback2(value closure, value arg1, value arg2);
extern value callback3(value closure, value arg1, value arg2, value arg3);

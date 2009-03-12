#include "mlvalues.h"

value interprete(int mode, bytecode_t bprog, CODE* rprog);
value callback(value closure, value arg);
value callback2(value closure, value arg1, value arg2);
value callback3(value closure, value arg1, value arg2, value arg3);

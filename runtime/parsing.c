/* The PDA automaton for parsers generated by camlyacc */

#include "config.h"
#include "mlvalues.h"
#include "memory.h"

struct parser_tables {    /* Mirrors parse_tables in ../mosmllib/Parsing.sml */
  value actions;
  value transl;
  char * lhs;
  char * len;
  char * defred;
  char * dgoto;
  char * sindex;
  char * rindex;
  char * gindex;
  value tablesize;
  char * table;
  char * check;
};

struct parser_env {       /* Mirrors parser_env in ../mosmllib/Parsing.sml */
  value s_stack;
  value v_stack;
  value symb_start_stack;
  value symb_end_stack;
  value stacksize;
  value curr_char;
  value lval;
  value symb_start;
  value symb_end;
  value sp;
  value rule_len;
  value rule_number;
};

#ifdef WORDS_BIGENDIAN
#define Short(tbl,n) \
  (*((unsigned char *)((tbl) + (n) * sizeof(short))) + \
          (*((schar *)((tbl) + (n) * sizeof(short) + 1)) << 8))
#else
#define Short(tbl,n) (((short *)(tbl))[n])
#endif

#ifdef DEBUG
int parser_trace = 0;
#define Trace(act) if(parser_trace) act
#else
#define Trace(act)
#endif

/* Input codes */
/* Mirrors parserInput in ../mosmllib/Parsing.sml */

#define SEMANTIC_ACTION_COMPUTED 0
#define STACKS_GROWN_1 1
#define STACKS_GROWN_2 2
#define START 3
#define TOKEN_READ 4

/* Output codes */
/* Mirrors parserOutput in ../mosmllib/Parsing.sml */

#define COMPUTE_SEMANTIC_ACTION Atom(0)
#define GROW_STACKS_1 Atom(1)
#define GROW_STACKS_2 Atom(2)
#define RAISE_PARSE_ERROR Atom(3)
#define READ_TOKEN Atom(4)

/* The pushdown automata */

value parse_engine(tables, env, cmd, arg) /* ML */
     struct parser_tables * tables;
     struct parser_env * env;
     value cmd;
     value arg;
{
  static int state;
  static mlsize_t sp;
  int n, n1, n2, m, state1;

  switch(Tag_val(cmd)) {

  case START:
    state = 0;
    sp = Int_val(env->sp);

  loop:
    Trace(printf("Loop %d\n", state));
    n = Short(tables->defred, state);
    if (n != 0) goto reduce;
    if (Int_val(env->curr_char) >= 0) goto testshift;
    return READ_TOKEN;
                                /* The ML code calls the lexer and updates */
                                /* symb_start and symb_end */
  case TOKEN_READ:
    env->curr_char = Field(tables->transl, Tag_val(arg));
    if (Wosize_val(arg) == 0) {
      env->lval = Val_long(0);
    } else {
      modify(&env->lval, Field(arg, 0));
    }
    Trace(printf("Token %d (0x%lx)\n", Int_val(env->curr_char), env->lval));

  testshift:
    n1 = Short(tables->sindex, state);
    n2 = n1 + Int_val(env->curr_char);
    if (n1 != 0 && n2 >= 0 && n2 <= Int_val(tables->tablesize) &&
        Short(tables->check, n2) == Int_val(env->curr_char)) goto shift;
    n1 = Short(tables->rindex, state);
    n2 = n1 + Int_val(env->curr_char);
    if (n1 != 0 && n2 >= 0 && n2 <= Int_val(tables->tablesize) &&
        Short(tables->check, n2) == Int_val(env->curr_char)) {
      n = Short(tables->table, n2);
      goto reduce;
    }
    return RAISE_PARSE_ERROR;
                                /* The ML code raises the Parse_error exn */
  shift:
    state = Short(tables->table, n2);
    Trace(printf("Shift %d\n", state));
    sp++;
    if (sp < Long_val(env->stacksize)) goto push;
    return GROW_STACKS_1;
                                /* The ML code resizes the stacks */
  case STACKS_GROWN_1:
  push:
    Field(env->s_stack, sp) = Val_int(state);
    modify(&Field(env->v_stack, sp), env->lval);
    Field(env->symb_start_stack, sp) = env->symb_start;
    Field(env->symb_end_stack, sp) = env->symb_end;
    env->curr_char = Val_int(-1);
    goto loop;

  reduce:
    Trace(printf("Reduce %d\n", n));
    m = Short(tables->len, n);
    env->sp = Val_int(sp);
    env->rule_number = Val_int(n);
    env->rule_len = Val_int(m);
    sp = sp - m + 1;
    m = Short(tables->lhs, n);
    state1 = Int_val(Field(env->s_stack, sp - 1));
    n1 = Short(tables->gindex, m);
    n2 = n1 + state1;
    if (n1 != 0 && n2 >= 0 && n2 <= Int_val(tables->tablesize) &&
        Short(tables->check, n2) == state1) {
      state = Short(tables->table, n2);
    } else {
      state = Short(tables->dgoto, m);
    }
    if (sp < Long_val(env->stacksize)) goto semantic_action;
    return GROW_STACKS_2;
                                /* The ML code resizes the stacks */
  case STACKS_GROWN_2:
  semantic_action:
    return COMPUTE_SEMANTIC_ACTION;
                                /* The ML code calls the semantic action */
  case SEMANTIC_ACTION_COMPUTED:
    Field(env->s_stack, sp) = Val_int(state);
    modify(&Field(env->v_stack, sp), arg);
    Field(env->symb_end_stack, sp) =
      Field(env->symb_end_stack, Int_val(env->sp));
    goto loop;
  }
  return Val_unit;		/* Can't reach return */
}

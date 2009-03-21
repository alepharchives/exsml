/* The "get_next_char" routine for lexers generated by camllex. */
#include <assert.h>

#include "mlvalues.h"
#include "stacks.h"
#include "str.h"
#include "interp.h"

struct lexer_buffer {
  value refill_buff;
  value lex_buffer;
  value lex_abs_pos;
  value lex_start_pos;
  value lex_curr_pos;
  value lex_last_pos;
  value lex_last_action;
};

value get_next_char(struct lexer_buffer *);

value get_next_char(struct lexer_buffer *lexbuf)
{
	mlsize_t buffer_len, curr_pos;

	buffer_len = string_length(lexbuf->lex_buffer);
	assert(VAL_TO_LONG(lexbuf->lex_curr_pos) > 0);
	curr_pos = (mlsize_t) (VAL_TO_LONG(lexbuf->lex_curr_pos));
	if (curr_pos >= buffer_len) {
		PUSH_ROOTS(r, 1);
		r[0] = (value) lexbuf;
		callback(lexbuf->refill_buff, (value) lexbuf);
		lexbuf = (struct lexer_buffer *) r[0];
		assert(VAL_TO_LONG(lexbuf->lex_curr_pos) > 0);
		curr_pos = (mlsize_t) (VAL_TO_LONG(lexbuf->lex_curr_pos));
		POP_ROOTS();
	}
	lexbuf->lex_curr_pos += 2;

	return INT_TO_VAL(Byte_u(lexbuf->lex_buffer, curr_pos));
}


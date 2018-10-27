#pragma once

#include <stdlib.h>
#include <stdbool.h>

enum
tok_type {
  tok_type_none = 0,
  tok_type_eof,
  tok_type_atom,
  tok_type_string,
  tok_type_int,
  tok_type_float,
  tok_type_symbol,
  tok_type_quote,
  tok_type_oparen,
  tok_type_cparen
};

enum
lex_err {
  lex_err_none,
  lex_err_string_eol,
  lex_err_string_eof,
  lex_err_symbol_eol,
  lex_err_symbol_eof,
  lex_err_symbol_space,
  lex_err_symbol_bracket
};

struct
pos {
  const char *const filename;
  char *ptr;
  size_t line;
  size_t column;
  size_t index;
};

struct
tok {
  struct pos pos;
  enum tok_type type;
  size_t length;
};

struct
lex_state {
  struct pos pos;
  const char *const str;
};

enum lex_err
lex(struct lex_state *const state, struct tok *const tok);

void
lex_print_err(const char *const msg,
              const struct lex_state *const state,
              const struct tok *const tok);

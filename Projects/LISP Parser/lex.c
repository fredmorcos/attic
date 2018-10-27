#include "lex.h"

#include <stdio.h>
#include <assert.h>
#include <ctype.h>

bool
lex_ateof(const struct lex_state *const state) {
  assert(state != NULL);
  return *state->pos.ptr == '\0';
}

void
lex_update(struct lex_state *const state) {
  assert(state != NULL);

  if (*state->pos.ptr == '\n') {
    state->pos.line++;
    state->pos.column = 1;
  } else {
    state->pos.column++;
  }

  state->pos.ptr++;
}

void
tok_set(const struct lex_state *const state, struct tok *const tok,
        const enum tok_type type, const size_t length) {
  assert(state != NULL);
  assert(tok != NULL);

  *tok = (struct tok) {
    .pos = (struct pos) {
      .filename = state->pos.filename,
      .ptr = state->pos.ptr,
      .line = state->pos.line,
      .column = state->pos.column,
      .index = state->pos.index
    },
    .type = type,
    .length = length
  };
}

void
lex_until(struct lex_state *const state, const char c) {
  if (!lex_ateof(state) && !(*state->pos.ptr == c)) {
    lex_update(state);
    lex_until(state, c);
  }
}

enum lex_err
lex(struct lex_state *const state, struct tok *const tok) {
  assert(state != NULL);
  assert(tok != NULL);

  switch(tok->type) {
  case tok_type_none:
    if (lex_ateof(state)) {
      tok_set(state, tok, tok_type_eof, 0);
    } else if (*state->pos.ptr == '(') {
      tok_set(state, tok, tok_type_oparen, 1);
      lex_update(state);
    } else if (*state->pos.ptr == ')') {
      tok_set(state, tok, tok_type_cparen, 1);
      lex_update(state);
    } else if (*state->pos.ptr == '\'') {
      tok_set(state, tok, tok_type_quote, 1);
      lex_update(state);
    } else if (isspace(*state->pos.ptr)) {
      lex_update(state);
      return lex(state, tok);
    } else if (*state->pos.ptr == '"') {
      tok_set(state, tok, tok_type_string, 1);
      lex_update(state);
      return lex(state, tok);
    } else if (*state->pos.ptr == ':') {
      tok_set(state, tok, tok_type_symbol, 1);
      lex_update(state);
      return lex(state, tok);
    } else if (*state->pos.ptr == ';') {
      lex_until(state, '\n');
      return lex(state, tok);
    } else {
      tok_set(state, tok, tok_type_atom, 1);
      lex_update(state);
      return lex(state, tok);
    }
    break;
  case tok_type_string:
    if (lex_ateof(state)) {
      return lex_err_string_eof;
    } else if (*state->pos.ptr == '\n') {
      return lex_err_string_eol;
    } else if (*state->pos.ptr == '"') {
      tok->length++;
      lex_update(state);
    } else {
      tok->length++;
      lex_update(state);
      return lex(state, tok);
    }
    break;
  case tok_type_symbol:
    if (lex_ateof(state)) {
      if (tok->length == 1) {
        return lex_err_symbol_eof;
      }
    } else if (*state->pos.ptr == '\n') {
      if (tok->length == 1) {
        return lex_err_string_eol;
      }
    } else if (isspace(*state->pos.ptr)) {
      if (tok->length == 1) {
        return lex_err_symbol_space;
      }
    } else if (*state->pos.ptr == '(' || *state->pos.ptr == ')') {
      if (tok->length == 1) {
        return lex_err_symbol_bracket;
      }
    } else {
      tok->length++;
      lex_update(state);
      return lex(state, tok);
    }
    break;
  default:
    if (!(lex_ateof(state) || isspace(*state->pos.ptr) ||
          *state->pos.ptr == '(' || *state->pos.ptr == ')')) {
      tok->length++;
      lex_update(state);
      return lex(state, tok);
    }
    break;
  }

  return lex_err_none;
}

const char *
lex_find_eol(const struct tok *const tok) {
  assert(tok != NULL);

  const char *c = tok->pos.ptr;

  while (*c != '\n' && *c != '\0') {
    c++;
  }

  return c;
}

const char *
lex_find_bol(const struct lex_state *const state,
             const struct tok *const tok) {
  assert(state != NULL);
  assert(tok != NULL);

  const char *c = tok->pos.ptr;

  while (*c != '\n' && c >= state->str) {
    c--;
  }

  return c + 1;
}

void
lex_print_err(const char *const msg,
              const struct lex_state *const state,
              const struct tok *const tok) {
  assert(msg != NULL);
  assert(state != NULL);
  assert(tok != NULL);

  const char *const begin = lex_find_bol(state, tok);
  const char *const end = lex_find_eol(tok);

  fprintf(stderr, "%s:%lu:%lu Error: %s (token starts at %lu:%lu)\n",
          state->pos.filename,
          (unsigned long) state->pos.line,
          (unsigned long) state->pos.column,
          msg,
          (unsigned long) tok->pos.line,
          (unsigned long) tok->pos.column);
  fprintf(stderr, "  %.*s\n", (int) (end - begin), begin);

  for (size_t i = 0; i < state->pos.column; i++) {
    fprintf(stderr, "-");
  }

  fprintf(stderr, "^\n");
}

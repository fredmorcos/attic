#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "lexer.h"

void tok_free(const struct tok *const tok) {
  assert(tok);

  if ((tok->type == TokRef ||
       tok->type == TokInvRef) &&
      tok->val.r.nelems)
    free(tok->val.r.elems);
}

static LexRet lex_ref(struct lex *const lex, struct tok *const tok) {
  assert(lex);
  assert(tok);

  const char *ptr = lex->ptr;
  size_t line = lex->line;
  size_t col = lex->col;

  if (*ptr == Nul) {
    lex->ptr = ptr, lex->line = line, lex->col = col;
    return LexRefEOF;
  }

  if (*ptr == ',' || isspace(*ptr)) {
    lex->ptr = ptr, lex->line = line, lex->col = col;
    return LexRefErr;
  }

  struct key *const new_elems =
    realloc(tok->val.r.elems,
            sizeof(struct key) *
            (tok->val.r.nelems + 1));

  if (!new_elems) {
    lex->ptr = ptr, lex->line = line, lex->col = col;
    return LexMemErr;
  }

  struct key *const new_key = new_elems + tok->val.r.nelems;
  new_key->type = KeyNormal;
  new_key->name = ptr;
  new_key->name_len = 0;

  tok->val.r.elems = new_elems;
  tok->val.r.nelems++;

  while (*ptr != '.' &&
         *ptr != ',' &&
         *ptr != Nul &&
         !isspace(*ptr)) {
    ptr++, col++;
    tok->tok_len++;
    new_key->name_len++;
  }

  if (*ptr == '.') {
    ptr++, col++;
    tok->tok_len++;
    lex->ptr = ptr, lex->line = line, lex->col = col;
    return lex_ref(lex, tok);
  }

  lex->ptr = ptr, lex->line = line, lex->col = col;
  return LexOK;
}

LexRet lex(struct lex *const lex, struct tok *const tok) {
  assert(lex);
  assert(tok);

  tok->file = lex->file;

  const char *ptr = lex->ptr;
  size_t line = lex->line;
  size_t col = lex->col;

  while (*ptr != Nul && isspace(*ptr)) {
    if (*ptr == Newline)
      line++, col = 0;
    else
      col++;
    ptr++;
  }

  const char c = *ptr;

  if (c == Nul) {                       /* eof */
    lex->ptr = ptr, lex->line = line, lex->col = col;
    return LexEnd;
  }

  if (strchr("{}[],:", c)) {
    tok->type =
      c == '{' ? TokLBrace :
      c == '}' ? TokRBrace :
      c == '[' ? TokLBracket :
      c == ']' ? TokRBracket :
      c == ',' ? TokComma :
      TokColon;

    tok->line = line;
    tok->col = col;
    tok->tok = ptr;
    tok->tok_len = 1;

    ptr++, col++;
    lex->ptr = ptr, lex->line = line, lex->col = col;

    return LexOK;
  }

  if (c == '#') {                       /* bool */
    tok->type = TokTrue;
    tok->line = line;
    tok->col = col;
    tok->tok = ptr;
    tok->tok_len = 2;

    ptr++, col++;

    const char c2 = *ptr;

    if (c2 == Nul) {                    /* unexpected eof */
      lex->ptr = ptr, lex->line = line, lex->col = col;
      return LexBoolEOF;
    }

    if (c2 != 't' && c2 != 'f') {       /* not t nor f */
      lex->ptr = ptr, lex->line = line, lex->col = col;
      return LexBoolErr;
    }

    tok->type = c2 == 't' ? TokTrue : TokFalse;

    ptr++, col++;
    lex->ptr = ptr, lex->line = line, lex->col = col;

    return LexOK;
  }

  if (c == '\"') {                      /* string */
    tok->type = TokStr;
    tok->line = line;
    tok->col = col;
    tok->tok = ptr;
    tok->tok_len = 1;

    ptr++, col++;

    while (*ptr != Nul && *ptr != '\"') {
      if (*ptr == Newline)
        line++, col = 0;
      else
        col++;
      ptr++;
      tok->tok_len++;
    }

    const char c2 = *ptr;

    if (c2 == Nul) {                    /* unexpected eof */
      lex->ptr = ptr, lex->line = line, lex->col = col;
      return LexStrEOF;
    }

    if (c2 != '\"') {                   /* some string error */
      lex->ptr = ptr, lex->line = line, lex->col = col;
      return LexStrErr;
    }

    tok->tok_len++;
    tok->val.s.ptr = tok->tok + 1;
    tok->val.s.len = tok->tok_len - 2;

    ptr++, col++;
    lex->ptr = ptr, lex->line = line, lex->col = col;

    return LexOK;
  }

  if (isdigit(c) || c == '-') {         /* ints and floats */
    tok->type = TokInt;
    tok->line = line;
    tok->col = col;
    tok->tok = ptr;
    tok->tok_len = 1;

    ptr++, col++;

    if (c == '-' && !isdigit(*ptr)) {
      lex->ptr = ptr, lex->line = line, lex->col = col;
      return LexNumErr;
    }

    while (isdigit(*ptr)) {
      ptr++, col++;
      tok->tok_len++;
    }

    char *end;

    if (*ptr == '.') {                  /* float */
      tok->type = TokFloat;
      ptr++, col++;
      tok->tok_len++;

      while (isdigit(*ptr)) {
        ptr++, col++;
        tok->tok_len++;
      }

      tok->val.f = strtod(tok->tok, &end);
      lex->ptr = ptr, lex->line = line, lex->col = col;

      return end == ptr ? LexOK : LexFloatErr;
    }

    tok->val.i = strtoll(tok->tok, &end, 10);
    lex->ptr = ptr, lex->line = line, lex->col = col;

    return end == ptr ? LexOK : LexIntErr;
  }

  if (c == '@' || c == '^') {           /* ref */
    tok->type = c == '@' ? TokRef : TokInvRef;
    tok->line = line;
    tok->col = col;
    tok->tok = ptr;
    tok->tok_len = 1;

    ptr++, col++;

    tok->val.r.elems = NULL;
    tok->val.r.nelems = 0;

    lex->ptr = ptr, lex->line = line, lex->col = col;

    switch(lex_ref(lex, tok)) {
    case LexBoolEOF:
    case LexBoolErr:
    case LexFloatErr:
    case LexIntErr:
    case LexNumErr:
    case LexStrEOF:
    case LexStrErr:
    case LexErr:
      return LexErr;
    case LexRefEOF:
      return LexRefEOF;
    case LexRefErr:
      return LexRefErr;
    case LexEnd:
      return LexErr;
    case LexMemErr:
      return LexMemErr;
    case LexOK:
      return LexOK;
    }
  }

  if (c != Nul && !isspace(c)) {        /* id */
    tok->type = TokID;
    tok->line = line;
    tok->col = col;
    tok->tok = ptr;
    tok->tok_len = 1;

    ptr++, col++;

    while (*ptr != ':' && *ptr != Nul && !isspace(*ptr)) {
      ptr++, col++;
      tok->tok_len++;
    }

    lex->ptr = ptr, lex->line = line, lex->col = col;
    return LexOK;
  }

  lex->ptr = ptr, lex->line = line, lex->col = col;
  return LexErr;
}

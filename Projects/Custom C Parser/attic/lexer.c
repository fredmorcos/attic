#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include "lexer.h"

void tok_free(const struct tok *const tok) {
  assert(tok);

  if ((tok->type == TokRef ||
       tok->type == TokInvRef) &&
      tok->val.r.nelems)
    free(tok->val.r.elems);
}

/*
 * static inline void tok(struct tok *const t,
 *                        const enum tok_type type,
 *                        const size_t line,
 *                        const size_t col,
 *                        const char *const ptr,
 *                        const size_t len) {
 *   assert(t);
 *   assert(ptr);
 *
 *   t->type = type;
 *   t->line = line;
 *   t->col = col;
 *   t->tok = ptr;
 *   t->tok_len = len;
 * }
 */

static inline char peek(const struct lex *const lex) {
  assert(lex);
  return *lex->ptr;
}

static inline char consume(struct lex *const lex) {
  assert(lex);
  const char c = peek(lex);
  if (c == Newline)
    lex->line++, lex->col = 0;
  else
    lex->col++;
  lex->ptr++;
  return c;
}

static inline void skip_space(struct lex *const lex) {
  assert(lex);
  while (peek(lex) != Nul && isspace(peek(lex)))
    (void) consume(lex);
}

static inline int findc(const char c, const char *s) {
  assert(s);
  while (*s != Nul) {
    if (*s == c)
      return 1;
    s++;
  }
  return 0;
}

/*
 * static LexRet lex_ref(struct lex *const lex, struct tok *const tok) {
 *   assert(lex);
 *   assert(tok);
 *
 *   const char c = peek(lex);
 *
 *   if (peek(lex) == Nul)
 *     return LexRefEOF;
 *
 *   if (c == ',' || isspace(c))
 *     return LexRefErr;
 *
 *   struct key *const new_elems =
 *     realloc(tok->val.r.elems,
 *             sizeof(struct key) *
 *             (tok->val.r.nelems + 1));
 *
 *   if (!new_elems) {
 *     st->ptr = ptr, st->line = line, st->col = col;
 *     return LexMemErr;
 *   }
 *
 *   struct key *const new_key = new_elems + tok->val.r.nelems;
 *   new_key->type = KeyNormal;
 *   new_key->name = ptr;
 *   new_key->namelen = 0;
 *
 *   tok->val.r.elems = new_elems;
 *   tok->val.r.nelems++;
 *
 *   while (*ptr != '.' &&
 *          *ptr != ',' &&
 *          *ptr != Nul &&
 *          !isspace(*ptr)) {
 *     ptr++, col++;
 *     tok->tok_len++;
 *     new_key->namelen++;
 *   }
 *
 *   if (*ptr == '.') {
 *     ptr++, col++;
 *     tok->tok_len++;
 *     st->ptr = ptr, st->line = line, st->col = col;
 *     return lex_ref(st, tok);
 *   }
 *
 *   st->ptr = ptr, st->line = line, st->col = col;
 *   return LexOK;
 * }
 */

LexRet lex(struct lex *const lex, struct tok *const tok) {
  assert(lex);
  assert(tok);

  skip_space(lex);

  const char c = peek(lex);

  if (c == Nul)                         /* eof */
    return LexEnd;

  tok->file = lex->file;

  if (findc(c, "{}[],:")) {             /* delimiters */
    tok->type =
      c == '{' ? TokLBrace :
      c == '}' ? TokRBrace :
      c == '[' ? TokLBracket :
      c == ']' ? TokRBracket :
      c == ',' ? TokComma :
      TokColon;
    tok->tok = lex->ptr;
    tok->tok_len = 1;
    (void) consume(lex);
    return LexOK;
  }

  if (c == '#') {                       /* bool */
    tok->tok = lex->ptr;
    tok->tok_len = 2;
    (void) consume(lex);

    const char c2 = consume(lex);

    if (c2 == Nul)                      /* unexpected eof */
      return LexBoolEOF;

    if (!findc(c2, "tf"))               /* not t nor f */
      return LexBoolErr;

    tok->type = c2 == 't' ? TokTrue : TokFalse;
    return LexOK;
  }

  if (c == '\"') {                      /* string */
    tok->type = TokStr;
    tok->tok = lex->ptr;
    tok->tok_len = 1;
    (void) consume(lex);

    char c2;

    do {
      c2 = consume(lex);
      tok->tok_len++;
    } while (c2 != Nul && c2 != '\"');

    if (c2 == Nul)                      /* unexpected eof */
      return LexStrEOF;

    if (c2 != '\"')                     /* some string error */
      return LexStrErr;

    tok->tok_len++;
    tok->val.s.ptr = tok->tok + 1;
    tok->val.s.len = tok->tok_len - 2;
    return LexOK;
  }

  if (isdigit(c) || c == '-') {         /* ints and floats */
    tok->type = TokInt;
    tok->tok = lex->ptr;
    tok->tok_len = 1;

    if (c == '-') {
      (void) consume(lex);
      if (!isdigit(peek(lex)))
        return LexNumErr;
    }

    while (isdigit(peek(lex))) {
      (void) consume(lex);
      tok->tok_len++;
    }

    if (peek(lex) == '.') {             /* float */
      tok->type = TokFloat;
      tok->tok_len++;
      (void) consume(lex);

      while (isdigit(peek(lex))) {
        tok->tok_len++;
        (void) consume(lex);
      }

      char *end;
      tok->val.f = strtod(tok->tok, &end);
      return end == lex->ptr ? LexOK : LexFloatErr;
    }

    char *end;
    tok->val.i = strtoll(tok->tok, &end, 10);
    return end == lex->ptr ? LexOK : LexIntErr;
  }

  if (findc(c, "@^")) {                 /* ref and inv_ref */
    tok->type = c == '@' ? TokRef : TokInvRef;
    tok->tok = lex->ptr;
    tok->tok_len = 1;

    tok->val.r.elems = NULL;
    tok->val.r.nelems = 0;

    (void) consume(lex);

    /*
     * switch(lex_ref(st, token)) {
     * case LexBoolEOF:
     * case LexBoolErr:
     * case LexFloatErr:
     * case LexIntErr:
     * case LexNumErr:
     * case LexStrEOF:
     * case LexStrErr:
     * case LexErr:
     *   return LexErr;
     * case LexRefEOF:
     *   return LexRefEOF;
     * case LexRefErr:
     *   return LexRefErr;
     * case LexEnd:
     *   return LexErr;
     * case LexMemErr:
     *   return LexMemErr;
     * case LexOK:
     *   return LexOK;
     * }
     */
  }

  if (c != Nul && !isspace(c)) {        /* id */
    tok->type = TokID;
    tok->tok = lex->ptr;
    tok->tok_len = 1;

    while ( != ':' && *ptr != Nul && !isspace(*ptr)) {
      ptr++, col++;
      token->tok_len++;
    }

    st->ptr = ptr, st->line = line, st->col = col;
    return LexOK;
  }

  st->ptr = ptr, st->line = line, st->col = col;
  return LexErr;
}

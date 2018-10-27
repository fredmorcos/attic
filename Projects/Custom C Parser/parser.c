#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

void tok_print(const struct tok *const tok) {
  assert(tok);

  printf("%s <%zu:%zu> ", tok->file, tok->line, tok->col);

  switch(tok->type) {
  case TokTrue:
  case TokFalse:
    printf("Bool");
    break;
  case TokLBrace:
  case TokRBrace:
  case TokLBracket:
  case TokRBracket:
  case TokComma:
  case TokColon:
    printf("Delim");
    break;
  case TokFloat:
    printf("Float");
    break;
  case TokInt:
    printf("Int");
    break;
  case TokID:
    printf("ID");
    break;
  case TokRef:
    printf("Ref");
    break;
  case TokInvRef:
    printf("IRef");
    break;
  case TokStr:
    printf("String");
    break;
  }

  printf(" %zu:(%.*s) --> ", tok->tok_len, (int) tok->tok_len, tok->tok);

  switch (tok->type) {
  case TokTrue:
    printf("TRUE");
    break;
  case TokFalse:
    printf("FALSE");
    break;
  case TokLBrace:
  case TokRBrace:
  case TokLBracket:
  case TokRBracket:
  case TokComma:
  case TokColon:
    printf("%c", *tok->tok);
    break;
  case TokFloat:
    printf("%f", tok->val.f);
    break;
  case TokInt:
    printf("%lld", tok->val.i);
    break;
  case TokID:
    printf("%zu:%.*s", tok->tok_len, (int) tok->tok_len, tok->tok);
    break;
  case TokStr:
    printf("%zu:%.*s",
           tok->val.s.len, (int) tok->val.s.len, tok->val.s.ptr);
    break;
  case TokRef:
    for (size_t i = 0; i < tok->val.r.nelems; i++) {
      const size_t keylen = tok->val.r.elems[i].name_len;
      const char *const key = tok->val.r.elems[i].name;
      printf("%zu:<%.*s>", keylen, (int) keylen, key);
    }
    break;
  case TokInvRef:
    for (size_t i = 0; i < tok->val.r.nelems; i++) {
      const size_t keylen = tok->val.r.elems[i].name_len;
      const char *const key = tok->val.r.elems[i].name;
      printf("%zu:<%.*s>", keylen, (int) keylen, key);
    }
    break;
  }

  printf("\n");
}

void lex_perr(const struct lex *const st,
              const struct tok *const tok,
              const char *const msg) {
  assert(st);
  assert(tok);
  assert(msg);

  fprintf(stderr, "%s <%zu:%zu> Error: %s\n",
          st->file, st->line, st->col, msg);
  fprintf(stderr, "  Token: %zu:%.*s\n",
          tok->tok_len, (int) tok->tok_len, tok->tok);
}

enum parser_ret {
  ParserOK = 1,
  ParserEnd = 0,
  ParserErr = -1,
  ParserMemErr = -2,
  ParserLexErr = -3,
  ParserFieldErr = -4,
  ParserObjErr = -5
};

typedef enum parser_ret ParserRet;

ParserRet parse_field(struct lex *const st,
                      struct tok *const tok,
                      LexRet *const lex_ret) {
  *lex_ret = lex(st, tok);

  if (*lex_ret == LexEnd)
    return ParserEnd;

  if (*lex_ret != LexOK)
    return ParserLexErr;

  if (tok->type != TokID)
    return ParserFieldErr;

  /* TODO: store property ID */

  *lex_ret = lex(st, tok);

  if (*lex_ret == LexEnd)
    return ParserEnd;

  if (*lex_ret != LexOK)
    return ParserLexErr;

  if (tok->type != TokColon)
    return ParserFieldErr;

  /* TODO: parse any primitive */
}

ParserRet parse_obj(struct lex *const st,
                    struct tok *const tok,
                    enum lex_ret *const lex_ret) {
  *lex_ret = lex(st, tok);

  if (*lex_ret == LexEnd)
    return ParserEnd;

  if (*lex_ret != LexOK)
    return ParserLexErr;

  if (tok->type != TokLBrace)
    return ParserObjErr;

 parse_prop_loop:
  /* TODO parse_prop() and store list of properties */

  *lex_ret = lex(st, tok);

  if (*lex_ret == LexEnd)
    return ParserEnd;

  if (*lex_ret != LexOK)
    return ParserLexErr;

  if (tok->type == TokComma)
    goto parse_prop_loop;

  if (tok->type == TokRBrace)
    return ParserOK;

  return ParserObjErr;
}

ParserRet parse(struct lex *const st,
                struct tok *const tok,
                enum lex_ret *const lex_ret) {
  return parse_obj(st, tok, lex_ret);
}

#include <sys/stat.h>

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "Error: No file given\n");
    return 1;
  }

  struct stat st;

  if (stat(argv[1], &st) == -1) {
    perror("Error: stat failed");
    return 2;
  }

  char *const data = malloc((size_t) st.st_size + 1);

  if (!data) {
    perror("Error: malloc failed");
    return 3;
  }

  FILE *const fp = fopen(argv[1], "r");

  if (!fp) {
    perror("Error: fopen failed");
    free(data);
    return 4;
  }

  if (fread(data, 1, (size_t) st.st_size, fp) < (size_t) st.st_size) {
    perror("Error: fread failed");
    free(data);
    fclose(fp);
    return 5;
  }

  fclose(fp);
  data[st.st_size] = '\0';

  struct lex state = (struct lex) { argv[1], data, data, 1, 0 };
  struct tok tok;

 lex_loop:
  switch(lex(&state, &tok)) {
  case LexBoolEOF:
    lex_perr(&state, &tok, "Unexpected EOF in bool");
    goto lex_error;
  case LexBoolErr:
    lex_perr(&state, &tok, "Cannot parse bool");
    goto lex_error;
  case LexFloatErr:
    lex_perr(&state, &tok, "Cannot parse float");
    goto lex_error;
  case LexIntErr:
    lex_perr(&state, &tok, "Cannot parse int");
    goto lex_error;
  case LexNumErr:
    lex_perr(&state, &tok, "Cannot parse number");
    goto lex_error;
  case LexRefEOF:
    lex_perr(&state, &tok, "Unexpected EOF in reference");
    goto lex_error;
  case LexRefErr:
    lex_perr(&state, &tok, "Cannot parse reference");
    goto lex_error;
  case LexStrEOF:
    lex_perr(&state, &tok, "Unexpected EOF in string");
    goto lex_error;
  case LexStrErr:
    lex_perr(&state, &tok, "Cannot parse string");
    goto lex_error;
  case LexMemErr:
    lex_perr(&state, &tok, "Memory allocation error");
    goto lex_error;
  case LexErr:
    lex_perr(&state, &tok, "Unknown error");
    goto lex_error;
  case LexOK:
    tok_print(&tok);
    tok_free(&tok);
    goto lex_loop;
  case LexEnd:
    break;
  }

  free((void *) state.buf);
  return 0;

 lex_error:
  tok_free(&tok);
  free((void *) state.buf);
  return 6;
}

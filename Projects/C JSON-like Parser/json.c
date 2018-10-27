#include <stdbool.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

enum jtype { JSTR, JINT, JSYM, JFLOAT, JBOOL, JID };

struct jlex_st {
  const char *ptr;
  size_t line, col;                     /* line and column */
};

struct jtok {
  enum jtype type;
  const char *ptr;
  size_t len;                           /* token length */
  size_t line, col;                     /* line and column */

  union {
    bool b;                             /* bool */
    double f;                           /* float */
    long long i;                        /* int */

    struct {
      const char *ptr;                  /* string */
      size_t len;                       /* string length */
    } str;
  } val;
};

inline void
jtok(struct jtok *const tok, const enum jtype type,
     const char *const ptr, const size_t len,
     const size_t line, const size_t col)
{
  *tok = (struct jtok) { type, ptr, len, line, col, {0} };
}

enum jlex {
  JLEX_ERR_NUM_FMT     = -6,
  JLEX_ERR_NUM         = -5,
  JLEX_ERR_SPEC_EOF    = -4,
  JLEX_ERR_STR_EOF     = -3,
  JLEX_ERR_STR_UNKNOWN = -2,
  JLEX_ERR             = -1,
  JLEX_END             =  0,
  JLEX_OK              =  1,
};

inline enum jlex
jlex(struct jlex_st *const st, struct jtok *const tok)
{
  static const char nul = '\0';
  static const char newline = '\n';

  const char *ptr = st->ptr;
  size_t line = st->line;
  size_t col = st->col;

  while (*ptr != nul && isspace(*ptr)) {
    if (*ptr == newline)
      line++, col = 0;
    else
      col++;
    ptr++;
  }

  const char c = *ptr;

  if (c == nul) {                       /* eof */
    *st = (struct jlex_st) { ptr, line, col };
    return JLEX_END;
  } else if (c == '{' || c == '}' ||
             c == '[' || c == ']' ||
             c == ',' || c == ':') {    /* symbols */
    jtok(tok, JSYM, ptr, 1, line, col);
    ptr++, col++;
    *st = (struct jlex_st) { ptr, line, col };
    return JLEX_OK;
  } else if (c == '#') {                /* bools */
    jtok(tok, JBOOL, ptr, 2, line, col);
    ptr++, col++;

    if (*ptr == nul) {
      *st = (struct jlex_st) { ptr, line, col };
      return JLEX_ERR_SPEC_EOF;
    } else if (*ptr == 't' || *ptr == 'f') {
      tok->val.b = *ptr == 't' ? true : false;
      ptr++, col++;
      *st = (struct jlex_st) { ptr, line, col };
      return JLEX_OK;
    } else {
      *st = (struct jlex_st) { ptr, line, col };
      return JLEX_ERR;
    }
  } else if (c == '\"') {               /* strings */
    jtok(tok, JSTR, ptr, 1, line, col);
    ptr++, col++;

    while (*ptr != nul && *ptr != '\"') {
      if (*ptr == newline)
        line++, col = 0;
      else
        col++;
      ptr++;
      tok->len++;
    }

    if (*ptr == nul) {
      *st = (struct jlex_st) { ptr, line, col };
      return JLEX_ERR_STR_EOF;
    } else if (*ptr == '\"') {
      tok->len++;
      tok->val.str.len = tok->len - 2;
      tok->val.str.ptr = tok->ptr + 1;
      ptr++, col++;
      *st = (struct jlex_st) { ptr, line, col };
      return JLEX_OK;
    } else {
      *st = (struct jlex_st) { ptr, line, col };
      return JLEX_ERR_STR_UNKNOWN;
    }
  } else if (isdigit(c) || c == '-') {  /* ints and floats */
    jtok(tok, JINT, ptr, 1, line, col);
    ptr++, col++;

    if (c == '-' && !isdigit(*ptr)) {
      *st = (struct jlex_st) { ptr, line, col };
      return JLEX_ERR_NUM;
    }

    while (isdigit(*ptr)) {
      ptr++, col++;
      tok->len++;
    }

    if (*ptr == '.') {
      tok->type = JFLOAT;
      ptr++, col++;
      tok->len++;

      while (isdigit(*ptr)) {
        ptr++, col++;
        tok->len++;
      }

      char *end;
      tok->val.f = strtof(tok->ptr, &end);
      *st = (struct jlex_st) { ptr, line, col };
      return end == ptr ? JLEX_OK : JLEX_ERR_NUM_FMT;
    } else {
      char *end;
      tok->val.i = strtoll(tok->ptr, &end, 10);
      *st = (struct jlex_st) { ptr, line, col };
      return end == ptr ? JLEX_OK : JLEX_ERR_NUM_FMT;
    }
  } else if (c != nul && !isspace(c)) { /* identifiers */
    jtok(tok, JID, ptr, 1, line, col);
    ptr++, col++;

    while (*ptr != ':' && *ptr != nul && !isspace(*ptr)) {
      ptr++, col++;
      tok->len++;
    }

    *st = (struct jlex_st) { ptr, line, col };
    return JLEX_OK;
  } else {
    *st = (struct jlex_st) { ptr, line, col };
    return JLEX_ERR;
  }
}

#if defined(LIBJSON_TEST)
#include <sys/stat.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>

int
main(int argc, char **argv)
{
 /*
  *  static const char *const jtype_str[] = {
  *    [JSTR]   = "String",
  *    [JINT]   = "Int",
  *    [JSYM]   = "Symbol",
  *    [JFLOAT] = "Float",
  *    [JBOOL]  = "Bool",
  *    [JID]    = "ID"
  *  };
  *
  *  struct jlex_st st =
  *    { " -2#t 22 {#f} #t \n 34.4 [ #t, #t, #f  ]bar : \"hello\" foo:\"world\" [] \"hello\":  \n 22.33 ,3.", 1, 0 };
  *  struct jtok tok;
  *
  * jlex_loop:
  *  switch (jlex(&st, &tok)) {
  *  case JLEX_END:
  *    printf("%.2zu:%.2zu Finished\n", st.line, st.col);
  *    return 0;
  *  case JLEX_OK:
  *    printf("%.2zu:%.2zu <%.2zu> %s",
  *           tok.line, tok.col, tok.len,
  *           jtype_str[tok.type]);
  *
  *    switch (tok.type) {
  *    case JSYM:   printf(" %c", *tok.ptr); break;
  *    case JSTR:   printf(" %.*s", (int) tok.val.str.len, tok.val.str.ptr); break;
  *    case JBOOL:  printf(" %d", (int) tok.val.b); break;
  *    case JINT:   printf(" %lld", tok.val.i); break;
  *    case JFLOAT: printf(" %f", tok.val.f); break;
  *    case JID:    printf(" %.*s", (int) tok.len, tok.ptr);
  *    }
  *    printf(" (%.*s)\n", (int) tok.len, tok.ptr);
  *    goto jlex_loop;
  *  default:
  *    fprintf(stderr, "%.2zu:%.2zu Error\n", st.line, st.col);
  *    return 1;
  *  }
  *
  *  return 0;
  */

  if (argc < 2)
    return 1;

  struct stat st;
  stat(argv[1], &st);

  char *const data = malloc(st.st_size);

  if (!data)
    return 2;

  FILE *fp = fopen(argv[1], "r");

  if (!fp) {
    free(data);
    return 3;
  }

  if (fread(data, 1, st.st_size, fp) < (size_t) st.st_size) {
    free(data);
    fclose(fp);
    return 4;
  }

  struct jlex_st lex_st = { data, 1, 0 };
  struct jtok tok;

  clock_t t = clock();

 jlex_loop:
  switch(jlex(&lex_st, &tok)) {
  case JLEX_END:
    break;
  case JLEX_OK:
    goto jlex_loop;
  default:
    fprintf(stderr, "%zu:%zu Error\n", lex_st.line, lex_st.col);
    free(data);
    fclose(fp);
    return 5;
  }

  printf("Elapsed CPU time: %.2Lfs\n",
         (clock() - t) / (long double) CLOCKS_PER_SEC);

  free(data);
  fclose(fp);
  return 0;
}
#endif

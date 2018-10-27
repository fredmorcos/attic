#include <stdbool.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>

enum jtok_type {
  JOBJ_BEGIN,
  JOBJ_END,
  JARR_BEGIN,
  JARR_END,
  JCOMMA,
  JCOLON,
  JSTR,
  JINT,
  JFLOAT,
  JBOOL,
  JCHAR,
  JNULL,
};

struct jtok {
  enum jtok_type type;

  const char *str;
  int len;
  size_t line;
  size_t col;

  union {
    int i;
    float f;
    bool b;
    char c;
    char *s;
  } val;
};

enum jlex_res {
  JLEX_OK,
  JLEX_ERR,
  JLEX_PREMCHAR,
  JLEX_PREMSTR,
  JLEX_END,
};

inline int
suntilc(const char **const p,
        size_t *const line,
        size_t *const col,
        const char c)
{
  int len = 0;

  while (**p != '\0' && **p == c) {
    if (**p == '\n')
      (*line)++, *col = 0;
    else
      (*col)++;

    (*p)++, len++;
  }

  return (**p == '\0' ? -1 : len);
}

inline int
suntilp(const char **const p,
        size_t *const line,
        size_t *const col,
        int (*pred)(int))
{
  int len = 0;

  while (**p != '\0' && pred(**p)) {
    if (**p == '\n')
      (*line)++, *col = 0;
    else
      (*col)++;

    (*p)++, len++;
  }

  return (**p == '\0' ? -1 : len);
}

inline enum jlex_res
jlex_upd(const enum jtok_type type,
         const char **const p,
         const int len,
         size_t *const line,
         size_t *const col,
         struct jtok *const res)
{
  *res = (struct jtok) { type, *p, len, *line, *col, {0} };
  (*col) += len;
  (*p) += len;
  return JLEX_OK;
}

enum jlex_res
jlex(const char **const p,
     size_t *const line,
     size_t *const col,
     struct jtok *const res)
{
  if (suntilp(p, line, col, isspace) == -1)
    return JLEX_END;

  switch (**p) {
  case '{': return jlex_upd(JOBJ_BEGIN, p, 1, line, col, res);
  case '}': return jlex_upd(JOBJ_END,   p, 1, line, col, res);
  case '[': return jlex_upd(JARR_BEGIN, p, 1, line, col, res);
  case ']': return jlex_upd(JARR_END,   p, 1, line, col, res);
  case ',': return jlex_upd(JCOMMA,     p, 1, line, col, res);
  case ':': return jlex_upd(JCOLON,     p, 1, line, col, res);
  case '\'': {
    (void) jlex_upd(JCHAR, p, 1, line, col, res);
    res->len += suntilc(p, line, col, '\'');

    if (res->len != 3)
      return JLEX_PREMCHAR;

    (*p)++;

    return JLEX_OK;
  }
  case '"': {
    (void) jlex_upd(JSTR, p, 1, line, col, res);
    res->len += suntilc(p, line, col, '"');

    if (res->len < 2)
      return JLEX_PREMSTR;

    (*p)++;

    return JLEX_OK;
  }
  }

  return JLEX_ERR;
}

#if defined(LIBJSON_TEST)
int
main()
{
  const char *buf = "   true  {}  false  \n  [] :   \" \n :   ,  null ";
  size_t line = 1;
  size_t col = 0;

  struct jtok res;

 json_lex_loop:
  switch (jlex(&buf, &line, &col, &res)) {
  case JLEX_OK:
    printf("%zu:%zu %.*s\n", res.line, res.col, res.len, res.str);
    goto json_lex_loop;
  case JLEX_PREMCHAR:
    printf("%zu:%zu: PREMCHAR\n", line, col);
    return 1;
  case JLEX_PREMSTR:
    printf("%zu:%zu: PREMSTR\n", line, col);
    return 1;
  case JLEX_ERR:
    printf("%zu:%zu: ERR\n", line, col);
    return 1;
  case JLEX_END:
    printf("%zu:%zu: END\n", line, col);
    return 0;
  }

  return 0;
}
#endif

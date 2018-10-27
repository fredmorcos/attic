#include "parser.h"
#include "extra.h"
#include "log.h"
#include "expense.h"
#include "buffer.h"
#include "file.h"
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>

void p_upd (char c, int *line, int *col);
bool p_ateof (buf_t *buf, char *p);

parser_ret p_ndigits (buf_t *ibuf, char **p, tok_t *tok,
                      char const **m, char const *e,
                      int *line, int *col, int n);
parser_ret p_until (buf_t *ibuf, char **p, tok_t *tok,
                    char const **m, char const *e,
                    int *line, int *col, int n,
                    char const *s, int (*f) (int));

const char *const parser_err[] = {
  [PARSER_OK]  = "Ok",
  [PARSER_EOF] = "Unexpected end of file",
  [PARSER_ERR] = "Could not parse token"
};

inline void p_upd (char c, int *line, int *col) {
  if (c == '\n') *col = 1, (*line)++; else (*col)++;
}

bool p_ateof (buf_t *buf, char *p) {
  return p >= (char *) buf->ptr + (buf->len * buf->esize);
}

parser_ret p_ndigits (buf_t *ibuf, char **p, tok_t *tok,
                      char const **m, char const *e,
                      int *line, int *col, int n) {
  int i = n;

  if (p_ateof(ibuf, *p)) { *m = e; return PARSER_EOF; }

  for (*tok = (tok_t) { ibuf, *p, 0, *line, *col };
       !p_ateof(ibuf, *p) && isdigit(**p) && i > 0;
       (*p)++, tok->len++, i--) {
    p_upd(**p, line, col);
  }

  if (tok->len != n) { *m = e; return PARSER_ERR; }

  return PARSER_OK;
}

parser_ret p_until (buf_t *ibuf, char **p, tok_t *tok,
                    char const **m, char const *e,
                    int *line, int *col, int n,
                    char const *s, int (*f) (int)) {
  if (p_ateof(ibuf, *p)) { *m = e; return PARSER_EOF; }

  for (*tok = (tok_t) { ibuf, *p, 0, *line, *col };
       !p_ateof(ibuf, *p) && (f ? !f(**p) : 1) && (s ? !iselem(**p, s) : 1);
       (*p)++, tok->len++) {
    p_upd(**p, line, col);
  }

  if (tok->len < n) { *m = e; return PARSER_ERR; }

  return PARSER_OK;
}

parser_ret parse_exps (file_t *ifile, buf_t *ebuf) {
  static const size_t n_tags = 4;

  int line = 1, col = 1;
  char *p = NULL;
  char *fn = NULL;
  char const *m = NULL;
  buf_t *ibuf = NULL;
  parser_ret r = PARSER_OK;
  exp_t e;
  tok_t tag;

  assert(ifile != NULL);
  assert(ifile->fn != NULL);
  assert(ifile->data != NULL);
  assert(ifile->data->ptr != NULL);
  assert(ebuf != NULL);
  assert(ebuf->ptr != NULL);

  ibuf = ifile->data;
  p = ifile->data->ptr;
  fn = ifile->fn;

  while (1) {
    memset(&e, 0, sizeof(exp_t));

    for (; !p_ateof(ibuf, p) && isspace(*p); p++) p_upd(*p, &line, &col);

    if (p_ateof(ibuf, p)) goto lv;

    for (e.amount = (tok_t) { ibuf, p, 0, line, col };
         !p_ateof(ibuf, p) && isdigit(*p);
         p++, e.amount.len++) {
      p_upd(*p, &line, &col);
    }

    if (!e.amount.len) { m = "amount"; goto lverr; }
    if (p_ateof(ibuf, p)) { m = "amount"; goto lveof; }

    if (*p == '.') {
      e.amount.len++;
      p_upd(*p, &line, &col);
      p++;

      if (p_ateof(ibuf, p)) { m = "amount"; goto lveof; }

      if (!isdigit(*p)) { m = "amount"; goto lverr; }

      e.amount.len++;
      p_upd(*p, &line, &col);
      p++;

      if (p_ateof(ibuf, p)) { m = "amount"; goto lveof; }

      if (isdigit(*p)) {
        e.amount.len++;
        p_upd(*p, &line, &col);
        p++;

        if (p_ateof(ibuf, p)) { m = "spaces"; goto lveof; }
      }
    }

    if (!isspace(*p)) { m = "spaces"; goto lverr; }
    for (; !p_ateof(ibuf, p) && isspace(*p); p++) p_upd(*p, &line, &col);

    switch (p_ndigits(ibuf, &p, &e.year, &m, "year", &line, &col, 4)) {
    case PARSER_EOF: goto lveof;
    case PARSER_ERR: goto lverr;
    case PARSER_OK:  break;
    }

    if (p_ateof(ibuf, p)) { m = "first `-` in date"; goto lveof; }

    if (*p == '-') {
      p_upd(*p, &line, &col);
      p++;
    } else { m = "first `-` in date"; goto lverr; }

    switch (p_ndigits(ibuf, &p, &e.month, &m, "month", &line, &col, 2)) {
    case PARSER_EOF: goto lveof;
    case PARSER_ERR: goto lverr;
    case PARSER_OK:  break;
    }

    if (p_ateof(ibuf, p)) { m = "second `-` in date"; goto lveof; }

    if (*p == '-') {
      p_upd(*p, &line, &col);
      p++;
    } else { m = "second `-` in date"; goto lverr; }

    switch (p_ndigits(ibuf, &p, &e.day, &m, "day", &line, &col, 2)) {
    case PARSER_EOF: goto lveof;
    case PARSER_ERR: goto lverr;
    case PARSER_OK:  break;
    }

    if (p_ateof(ibuf, p)) { m = "spaces"; goto lveof; }
    for (; !p_ateof(ibuf, p) && isspace(*p); p++) p_upd(*p, &line, &col);

    switch (p_until(ibuf, &p, &e.person, &m, "person",
                    &line, &col, 1, NULL, isspace)) {
    case PARSER_EOF: goto lveof;
    case PARSER_ERR: goto lverr;
    case PARSER_OK:  break;
    }

    if (p_ateof(ibuf, p)) { m = "spaces"; goto lveof; }
    for (; !p_ateof(ibuf, p) && isspace(*p); p++) p_upd(*p, &line, &col);

    switch (p_until(ibuf, &p, &e.shop, &m, "shop",
                    &line, &col, 1, NULL, isspace)) {
    case PARSER_EOF: goto lveof;
    case PARSER_ERR: goto lverr;
    case PARSER_OK:  break;
    }

    if (p_ateof(ibuf, p)) { m = "spaces"; goto lveof; }
    for (; !p_ateof(ibuf, p) && isspace(*p); p++) p_upd(*p, &line, &col);

    if (!(e.tags_buf = buf_init(NULL, n_tags, sizeof(tok_t)))) {
      m = "tags buffer";
      goto lval;
    }

    for (; !p_ateof(ibuf, p) && !isspace(*p);) {
      switch (p_until(ibuf, &p, &tag, &m, "tag",
                      &line, &col, 1, ",", isspace)) {
      case PARSER_EOF: break;
      case PARSER_ERR: goto lverr;
      case PARSER_OK:  break;
      }

      if (!buf_append(e.tags_buf, &tag)) { m = "tag token"; goto lval; }
      if (p_ateof(ibuf, p)) break;

      if (*p == ',') {
        p_upd(*p, &line, &col);
        p++;
      }
    }

    if (!e.tags_buf->len) { m = "tags"; goto lverr; }

    exp_sort_tags(e.tags_buf);

    for (; !p_ateof(ibuf, p) && isspace(*p) && *p != '\n';
         p++) p_upd(*p, &line, &col);

    switch (p_until(ibuf, &p, &e.note, &m, "note",
                    &line, &col, 0, "\n", NULL)) {
    case PARSER_EOF: break;
    case PARSER_ERR: goto lverr;
    case PARSER_OK:  break;
    }

    if (!buf_append(ebuf, &e)) { m = "expense"; goto lval; }
  }

 lv:
  return r;
 lverr:
  r = PARSER_ERR;
  log_err("Cannot parse %s at %s:%d:%d: %s", m, fn, line, col, parser_err[r]);
  goto lv;
 lveof:
  r = PARSER_EOF;
  log_err("Cannot parse %s at %s:%d:%d: %s", m, fn, line, col, parser_err[r]);
  goto lv;
 lval:
  r = PARSER_ERR;
  log_syserr(errno, "Cannot allocate memory for %s", m);
  goto lv;
}

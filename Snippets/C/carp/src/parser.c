#include <carp/parser.h>
#include <buffer/buffer.h>
#include <talloc/talloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <assert.h>
#include <errno.h>

typedef int(pred_fp)(int);

inline bool ateof(struct carp_parser *parser);
inline void update(struct carp_parser *parser);
inline bool skip(struct carp_parser *parser, pred_fp *pred);

inline int isnewline(int c);
inline int isnotnewline(int c);
inline int isspaceorcolon(int c);
inline int issinglequote(int c);
inline int isdoublequote(int c);
inline int isdot(int c);
inline int isnotdigit(int c);
inline int ispound(int c);

int isnewline(int c) {
  return c == '\n';
}

int isnotnewline(int c) {
  return c != '\n';
}

int isspaceorcolon(int c) {
  return isspace(c) || c == ':';
}

int issinglequote(int c) {
  return c == '\'';
}

int isdoublequote(int c) {
  return c == '"';
}

int isdot(int c) {
  return c == '.';
}

int isnotdigit(int c) {
  return !isdigit(c);
}

int ispound(int c) {
  return c == '#';
}

bool ateof(struct carp_parser *parser) {
  return parser->p >= (char *) parser->buf->ptr +
    (parser->buf->len * parser->buf->element_size);
}

void update(struct carp_parser *parser) {
  if (*parser->p == '\n') {
    parser->col = 1;
    parser->line++;
  } else {
    parser->col++;
  }

  parser->p++;
  parser->idx++;
}

struct carp_parser_token *token(struct carp_parser *parser) {
  const size_t tok_len = sizeof(struct carp_parser_token);
  struct carp_parser_token *tok = NULL;

  assert(parser != NULL);

  if ((tok = talloc(tok_len, NULL)) == NULL) {
    fprintf
      (stderr,
       "Error: Parser: Allocation of %lu bytes for token failed: %s\n",
       (unsigned long) tok_len, strerror(errno));
    return NULL;
  }

  tok->str  = parser->p;
  tok->len  = 0;
  tok->line = parser->line;
  tok->col  = parser->col;

  return tok;
}

bool skip(struct carp_parser *parser, pred_fp *pred) {
  bool res = false;

  while (!ateof(parser) && pred(*parser->p)) {
    update(parser);
    res = true;
  }

  return res;
}

struct carp_parser_token *until(struct carp_parser       *parser,
                                struct carp_parser_token *token,
                                pred_fp                  *pred,
                                bool                      empty_ok,
                                bool                      eof_ok) {
  bool empty = true;
  struct carp_parser prev_state;

  assert(parser   != NULL);
  assert(token    != NULL);
  assert(pred     != NULL);

  /* Note  that  we  store  the   previous  state  of  the  parser  in
   * `err_info`. In  case of a  parse failure  we will have  to revert
   * back.
   */
  memcpy(&prev_state, parser, sizeof(struct carp_parser));

  /* All we  have to do  now is keep  updating the `token`  length and
   * incrementing  the  parsing pointer  `p`  as  long as  we  haven't
   * reached  the end  or a  character  that results  in `false`  from
   * `pred`. Of course that is all except in a case of parsing error.
   */
  while (!ateof(parser) && !pred(*parser->p)) {
    update(parser);
    token->len++;
    empty = false;
  }

  if ((!empty_ok && empty) || (!eof_ok && ateof(parser))) {
    /* Revert parser state back and exit with a parsing error. */
    memcpy(parser, &prev_state, sizeof(struct carp_parser));
    return NULL;
  }

  return token;
}

struct carp_parser_token *directive(struct carp_parser *parser,
                                    struct carp_parser_token *tok) {

  const char *const directive_map[] = {
    [CARP_PARSER_TOKEN_DIRECTIVE_DEFINE]  = ".define",
    [CARP_PARSER_TOKEN_DIRECTIVE_ADDRESS] = ".address"
  };

  assert(parser != NULL);
  assert(tok != NULL);

  if(!isdot(*parser->p)) {
    return NULL;
  }

  if (until(parser, tok, isspace, false, false) == NULL) {
    fprintf(stderr, "Error: Parser: Expected a directive\n");
    return NULL;
  }

  /* Note that sizeof(directive_map) here is the number of elements in
   * the  array.  Also, the  directive  strings  in directive_map  are
   * literal strings that are automatically NULL-terminated.
   */
  for (size_t i = 0; i < sizeof(directive_map); i++) {
    if (strncasecmp(tok->str, directive_map[i], tok->len) == 0) {
      tok->type = i;
      return tok;
    }
  }

  /* Unknown directive */
  fprintf(stderr, "Error: Parser: Unrecognized directive: %.*s\n",
          (int) tok->len, tok->str);
  return NULL;
}

struct carp_parser_token *identifier(struct carp_parser *parser,
                                     struct carp_parser_token *tok) {
  assert(parser != NULL);
  assert(tok != NULL);

  if (!isalpha(*parser->p)) {
    return NULL;
  }

  if (until(parser, tok, isspaceorcolon, false, false) == NULL) {
    fprintf(stderr, "Error: Parser: Expected an identifier\n");
    return NULL;
  }

  tok->type = CARP_PARSER_TOKEN_IDENTIFIER;
  return tok;
}

struct carp_parser_token *literal(struct carp_parser *parser,
                                  struct carp_parser_token *tok) {
  assert(parser != NULL);
  assert(tok != NULL);

  if (issinglequote(*parser->p)) {
    /* Character literal */
    if (until(parser, tok, issinglequote, false, false) == NULL) {
      fprintf(stderr, "Error: Parser: Could not parse character literal\n");
      return NULL;
    }

    update(parser);
    tok->len++;
    tok->type = CARP_PARSER_TOKEN_LITERAL_CHARACTER;
    return tok;
  } else if (isdoublequote(*parser->p)) {
    /* String literal */
    if (until(parser, tok, isdoublequote, false, false) == NULL) {
      fprintf(stderr, "Error: Parser: Could not parse string literal\n");
      return NULL;
    }

    update(parser);
    tok->len++;
    tok->type = CARP_PARSER_TOKEN_LITERAL_STRING;
    return tok;
  } else if (isdigit(*parser->p)) {
    /* Number */
    if (until(parser, tok, isnotdigit, false, false) == NULL) {
      fprintf(stderr, "Error: Parser: Could not parse number literal\n");
      return NULL;
    }

    if (isdot(*parser->p)) {
      /* Float */
      struct carp_parser_token newtok;

      tok->len++;
      update(parser);

      if (until(parser, &newtok, isnotdigit, false, false) == NULL) {
        fprintf(stderr, "Error: Parser: Could not parse float literal\n");
        return NULL;
      } else {
        tok->len += newtok.len;
        tok->type = CARP_PARSER_TOKEN_LITERAL_FLOAT;
      }
    } else {
      /* Integer */
      tok->type = CARP_PARSER_TOKEN_LITERAL_INTEGER;
      return tok;
    }
  } else {
    fprintf(stderr, "Error: Parser: Expected a literal\n");
    return NULL;
  }

  return NULL;
}

struct carp_parser_token *carp_parser_parse(struct carp_parser *parser) {
  struct carp_parser_token
    *top_tok = NULL,                   /* Top of AST tree */
    *cur_tok = NULL,
    *par_tok = NULL;

  assert(parser           != NULL);
  assert(parser->buf      != NULL);
  assert(parser->buf->ptr != NULL);
  assert(parser->buf->len >  0);
  assert(parser->p        == parser->buf->ptr);

  if ((top_tok = token(parser)) == NULL) {
    return NULL;
  }

  if ((cur_tok = token(parser)) == NULL) {
    tfree(top_tok);
    return NULL;
  }

  par_tok = top_tok;

  while (1) {
    if (ateof(parser)) {
      break;
    }

    (void) skip(parser, isspace);

    if (ateof(parser)) {
      break;
    }

    if (ispound(*parser->p)) {
      /* If it's a comment, skip it */
      (void) skip(parser, isnotnewline);
    } else if (isdot(*parser->p)) {
      if (directive(parser, cur_tok) == NULL) {
      }
    }

    if (ateof(parser)) {
      break;
    }

    (void) skip(parser, isspace);

    if (ateof(parser)) {
      break;
    }
  }

  return top_tok;
}

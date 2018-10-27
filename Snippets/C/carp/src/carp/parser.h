#ifndef CARP_PARSER_H
#define CARP_PARSER_H

#include <buffer/buffer.h>

enum carp_parser_token_type {
  CARP_PARSER_TOKEN_DIRECTIVE_DEFINE,
  CARP_PARSER_TOKEN_DIRECTIVE_ADDRESS,
  CARP_PARSER_TOKEN_IDENTIFIER,
  CARP_PARSER_TOKEN_LITERAL_STRING,
  CARP_PARSER_TOKEN_LITERAL_INTEGER,
  CARP_PARSER_TOKEN_LITERAL_CHARACTER,
  CARP_PARSER_TOKEN_LITERAL_FLOAT
};

struct carp_parser {
  struct buffer *buf;
  char          *p;
  size_t         line;
  size_t         col;
  size_t         idx;
};

struct carp_parser_token {
  enum carp_parser_token_type  type;
  char                        *str;
  size_t                       len;
  size_t                       line;
  size_t                       col;

  union {
    struct {
      char   *ptr;
      size_t  len;
    } str_val;
    char    char_val;
    double  float_val;
    int     int_val;
  };
};

struct carp_parser_token *carp_parser_parse(struct carp_parser *parser);

#endif

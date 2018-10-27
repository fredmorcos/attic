#ifndef PET_PARSER_H
#define PET_PARSER_H

#include "buffer.h"
#include "file.h"
#include <stdbool.h>

typedef struct {
  buf_t *ibuf;          /* the original input buffer */
  char  *ptr;           /* pointer to the token in the input buffer */
  int    len;           /* the length of the token */
  int    line;          /* position:line of token in input buffer */
  int    col;           /* position:column of token in input buffer */
} tok_t;

typedef enum {
  PARSER_OK = 0,
  PARSER_EOF,
  PARSER_ERR
} parser_ret;

extern const char *const parser_err[];

parser_ret parse_exps (file_t *ifile, buf_t *ebuf);

#endif

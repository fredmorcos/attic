#include "parser.h"
#include <stdint.h>
#include <inttypes.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

void parser_update (Parser *p) {
  if (p->input[p->pos] == '\n') {
    (p->line)++;
    p->col = 1;
  } else {
    (p->col)++;
  }
}

void parser_skip_spaces (Parser *p, uintmax_t *len) {
  *len = 0;

  while (isspace(p->input[p->pos]) != 0 && p->input[p->pos] != '\n') {
    (*len)++;
    parser_update(p);
    (p->pos)++;
  }
}

void parser_skip_newlines (Parser *p, uintmax_t *len) {
  *len = 0;

  while (p->input[p->pos] == '\n') {
    (*len)++;
    parser_update(p);
    (p->pos)++;
  }
}

void parser_skip_spaces_newlines (Parser *p, uintmax_t *len) {
  *len = 0;

  while (isspace(p->input[p->pos])) {
    (*len)++;
    parser_update(p);
    (p->pos)++;
  }
}

void parser_satisfy (Parser *p, uintmax_t *len, uint8_t (*pred) (char)) {
  *len = 0;

  while (pred(p->input[p->pos])) {
    (*len)++;
    parser_update(p);
    (p->pos)++;
  }
}

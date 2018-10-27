#include "vcard.h"

#include <stdio.h>
#include <stdbool.h>
#include <ctype.h>
#include <assert.h>

void string_assert(const struct string *const string) {
  assert(string != NULL);
  assert(string->str != NULL);
}

void parser_assert(const struct parser *const parser) {
  assert(parser != NULL);

  string_assert(parser->buffer);

  assert(parser->p != NULL);
  assert(parser->p >= parser->buffer->str);
}

int parser_print_state(const struct parser *const parser) {
  return fprintf(stderr,
                 "Parser:\n"
                 "  *buffer = %p\n"
                 "  *p      = %p\n"
                 "   line   = %zu\n"
                 "   col    = %zu\n",
                 (void *) parser->buffer,
                 (void *) parser->p,
                 parser->line,
                 parser->col);
}

void parser_upd_pos(struct parser *const parser) {
  parser_assert(parser);

  if (*parser->p == '\n') {
    parser->line++;
    parser->col = 1;
  } else {
    parser->col++;
  }
}

bool parser_at_end(const struct parser *const parser) {
  parser_assert(parser);

  char c = *parser->p;

  return parser->p == parser->buffer->str + (parser->buffer->len * sizeof(c)) ||
    c == '\0';
}

size_t parser_skip_ws(struct parser *const parser) {
  parser_assert(parser);

  size_t skipped = 0;

  while (!parser_at_end(parser) && isspace(*parser->p)) {
    parser_upd_pos(parser);
    skipped++;
    parser->p++;
  };

  return skipped;
}

int vcard_parse(const struct string *const buffer,
                struct string **vcard) {
  struct parser parser = {
    .buffer = buffer,
    .p = buffer->str,
    .line = 1,
    .col = 1
  };

  parser_print_state(&parser);

  parser_skip_ws(&parser);

  if (parser_at_end(&parser)) {
    printf("true\n");
  } else {
    printf("false\n");
  }

  parser_print_state(&parser);

  return 0;
}

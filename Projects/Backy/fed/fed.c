#include <assert.h>
#include <ctype.h>
#include <err.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdint.h>
#include <limits.h>
#include <inttypes.h>
#include <sysexits.h>
#include <unistd.h>

enum token_type {
  TOK_OP,
  TOK_CP,
  TOK_ID
};

struct token {
  enum token_type type;
  const char *value;
  size_t length;
};

const char *lex(const char *input, struct token *const tok) {
  /* skip whitespace */
  while (isspace(*input))
    input++;

  if (*input == '\0') {
    return NULL;
  } else if (*input == '(') {
    *tok = (struct token) { .type = TOK_OP, .value = input, .length = 1 };
  } else if (*input == ')') {
    *tok = (struct token) { .type = TOK_CP, .value = input, .length = 1 };
  } else {
    tok->type = TOK_ID;
    tok->value = input;
    tok->length = 0;

    while (isalnum(*input) || ispunct(*input))
      tok->length++, input++;
  }

  input++;

  return input;
}

int main(void) {
  FILE *input;
  char *buf = NULL;
  size_t buflen = 0;

  if ((input = open_memstream(&buf, &buflen)) == NULL)
    err(EX_OSERR, "Error creating memory stream");

  for (int c; (c = fgetc(stdin)) != EOF;)
    fputc(c, input);

  if (ferror(stdin) != 0) {
    warn("Error reading from stdin");
    fclose(input);
    free(buf);
    return EX_OSERR;
  }

  fputc('\0', input);

  fclose(input);

  struct token tok;

  for (const char *p = buf; (p = lex(p, &tok));)
    printf("%.*s\n", (int) tok.length, tok.value);

  free(buf);

  return EX_OK;
}

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

struct parser_state {
  char *buf;                        /* pointer to original position */
  char *ptr;                        /* pointer to current position */
  size_t pos;                       /* character position in stream */
  size_t len;                       /* buffer length */
  size_t line;                      /* current line position */
  size_t col;                       /* current column position */
};

typedef struct parser_state ParserState;

enum token_type {
  TOKEN_TYPE_NONE = 0,
  TOKEN_TYPE_SYM,
  TOKEN_TYPE_ID,
  TOKEN_TYPE_EOF,
  TOKEN_TYPE_LAST
};

typedef enum token_type TokenType;

struct token {
  TokenType type;                      /* token type */
  char *ptr;                           /* pointer to token position */
  size_t pos;                          /* token position in stream */
  size_t len;                          /* token length */
  size_t line;                         /* token line position */
  size_t col;                          /* token column position */
};

typedef struct token Token;

void aslc_fclose(FILE *const fd, const char *const filename);
char *aslc_fread(FILE *const fd, size_t *const len, size_t *const used_len);

void aslc_fclose(FILE *const fd, const char *const filename) {
  if (fclose(fd) != 0) {
    fprintf(stderr,
            "[W] Cannot close file `%s': %s\n",
            filename, strerror(errno));
  }
}

char *aslc_fread(FILE *const fd, size_t *const len, size_t *const used_len) {
  const size_t chunk_len = 4096 * sizeof(char);

  char *buf = NULL;
  char *newbuf = NULL;

  size_t buf_len = 0;
  size_t buf_usedlen = 0;

  do {
    size_t fread_num = 0;

    newbuf = realloc(buf, buf_len + chunk_len + 1);

    if (newbuf == NULL) {
      if (buf)
        free(buf);

      return NULL;
    }

    buf = newbuf;
    buf_len += chunk_len;

    fread_num = fread(&buf[buf_usedlen], sizeof(char), chunk_len, fd);

    buf_usedlen += fread_num;

    if (fread_num < chunk_len) {
      if (feof(fd) != 0)
        break;

      if (ferror(fd) != 0) {
        int tmp_errno = errno;
        free(buf);
        errno = tmp_errno;
        return NULL;
      }
    }
  } while (true);

  if (len != NULL)
    *len = buf_len + 1;

  if (used_len != NULL)
    *used_len = buf_usedlen;

  buf[buf_usedlen] = '\0';

  return buf;
}

bool aslc_is_sym(const char c) {
  return c == '(' || c == ')' || c == '@' || c == '$';
}

Token *aslc_lex(ParserState *const state) {
  Token *tok = NULL;
  TokenType type = TOKEN_TYPE_NONE;

  if (state->pos == state->len && *(state->ptr) != '\0')
    return NULL;

  if (state->pos == state->len && *(state->ptr) == '\0')
    type = TOKEN_TYPE_EOF;
  else if (aslc_is_sym(*(state->ptr)))
    type = TOKEN_TYPE_SYM;
  else
    return aslc_lex_id(state);

  tok = malloc(sizeof(Token));

  if (tok == NULL)
    return NULL;

  tok->
}

Token **aslc_parse(const char *const buf) {
}

int main (int argc, char **argv) {
  const char *fn = NULL;
  FILE *fd = NULL;
  size_t len = 0;
  char *buf = NULL;

  if (argc > 2) {
    fprintf(stderr, "[E] Only one file can be compiled at a time.\n");
    return EXIT_FAILURE;
  }

  fn = argv[1];
  fd = fopen(fn, "r");

  if (fd == NULL) {
    fprintf(stderr,
            "[E] Cannot open file `%s' for reading: %s\n",
            fn, strerror(errno));
    return EXIT_FAILURE;
  }

  buf = aslc_fread(fd, NULL, &len);

  if (buf == NULL) {
    fprintf(stderr,
            "[E] Cannot read file `%s': %s\n",
            fn, strerror(errno));
    aslc_fclose(fd, fn);
    return EXIT_FAILURE;
  }

  aslc_fclose(fd, fn);

  printf("[C] %s\n", fn);
  /* XXX parsing code here */
  free(buf);

  return EXIT_SUCCESS;
}

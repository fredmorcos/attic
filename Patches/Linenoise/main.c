#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include "linenoise.h"

#define strcmpn(s1, s2) strncmp(s1, s2, strlen(s2))

enum Requirement {
  Required,
  Optional
};

enum Type {
  String,
  Integer,
  List
};

union Value {
  char* string;
  int integer;
  struct ast_t** items;
};

struct ast_t {
  char* token;
  unsigned line;
  unsigned col;
  enum Type type;
  union Value value;
};

struct parser_t {
  char* input;
  char* p;
  unsigned line;
  unsigned col;
};

struct parser_err_t {
  char* msg;
  struct parser_t* parser;
};

int parser_is_eof (struct parser_t *parser) {
  char c;

  assert(parser != NULL);

  c = *parser->p;

  return c == '\0' || c == EOF ? 1 : 0;
}

struct parser_err_t* parser_skip_spaces (struct parser_t *parser,
                                         enum Requirement req,
                                         int newlines) {
  char c;
  int seen = 0;

  assert(parser != NULL);

  if (parser_is_eof(parser) && req == Required) {
    struct parser_err_t* err = (struct parser_err_t*)
      malloc (sizeof(struct parser_err_t));
    char* fmt = "Unexpected end of file, expected a spacing%s";
    char* fmt_ext = " or a newline";

    err->msg = (char*)
      malloc(sizeof(char) * (strlen(fmt) + strlen(fmt_ext) + 1));
    sprintf(err->msg, fmt, newlines == 0 ? "" : fmt_ext);
    err->parser = parser;

    return err;
  }

  c = *parser->p;

  while (isspace(c)) {
    if (c == '\n' && newlines == 0) {
      break;
    }

    seen = 1;

    if (c == '\n') {
      parser->line++;
      parser->col = 1;
    } else {
      parser->col++;
    }

    parser->p++;
    c = *parser->p;
  }

  if (req == Required && seen == 0) {
    struct parser_err_t* err = (struct parser_err_t*)
      malloc (sizeof(struct parser_err_t));
    char* fmt = "Unexpected character `%c`, expected a spacing%s";
    char* fmt_ext = " or a newline";

    err->msg = (char*)
      malloc(sizeof(char) * (strlen(fmt) + strlen(fmt_ext) + 2));
    sprintf(err->msg, fmt, *parser->p, newlines == 0 ? "" : fmt_ext);
    err->parser = parser;

    return err;
  }

  return NULL;
}

struct parser_err_t* parser_parse_int (struct parser_t* parser,
                                       enum Requirement req,
                                       struct ast_t* ast) {
  char c;

  assert(parser != NULL);
  assert(ast == NULL);

  c = *parser->p;

  return NULL;
}

/* int parser_parse_string (struct parser_t* parser, struct ast_t* ast) { */
/*   int count = 0; */

/*   assert(parser != NULL); */
/*   assert(ast == NULL); */

/*   /\* find length until corresponding double-quote *\/ */
/*   if (*parser->p != '"') { */
/*     return -1; */
/*   } */

/*   ast = (struct ast_t*) malloc (sizeof(struct ast_t)); */
/* } */

/* int parser_parse_value (struct parser_t* parser, struct ast_t* ast) { */
/*   int ret; */

/*   parser_skip_spaces(parser); */

/*   if (*parser->p == '"') { */
/*     /\* string *\/ */
/*   } else { */
/*     /\* integer *\/ */
/*   } */

/*   return 0; */
/* } */

/* int parser_parse_expression (struct parser_t* parser, struct ast_t* ast) { */
/*   int ret; */

/*   parser_skip_spaces(parser); */

/*   if (*parser->p == '(') { */
/*     /\* list *\/ */
/*   } else { */
/*     /\* value *\/ */
/*   } */

/*   return 0; */
/* } */

int main (void) {
  struct ast_t toplevel =  { .token = NULL,
                             .line  = 1,
                             .col   = 1,
                             .type  = List,
                             .value = { .items = NULL } };
  struct parser_t parser = { .input = NULL,
                             .p     = NULL,
                             .line  = 1,
                             .col   = 1 };

  struct parser_err_t* parser_err = NULL;

  puts("My LISP Computing Environment");
  puts("Type Ctrl+c to exit\n");

  while ((parser.input = linenoise("USER> ")) != NULL) {
    if (parser.input[0] == EOF  ||
        strcmpn(parser.input, "exit") == 0) {
      break;
    }

    if (parser.input[0] == '\0') {
      continue;
    }

    linenoiseHistoryAdd(parser.input);
    printf(" >> %s\n", parser.input);

    parser.p = parser.input;

    parser_err = parser_skip_spaces(&parser, Required, 1);

    if (parser_err) {
      fprintf(stderr, "Error at line %u, column %u\n",
              parser.line, parser.col);
      fprintf(stderr, "  -> %s\n", parser_err->msg);
      free(parser_err->msg);
      free(parser_err);
    }

    parser.line = 1;
    parser.col = 1;
    free(parser.input);
  }

  return EXIT_SUCCESS;
}

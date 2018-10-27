#include "lex.h"
#include "file.h"

#include <stdio.h>
#include <assert.h>
#include <talloc.h>

enum
parse_err {
  parse_err_none,
  parse_err_unmatched_paren
};

struct
ast {
  struct tok *tokens;
  struct ast *children;
};

enum parse_err
parse_atom(const struct lex_state *const state, struct ast *const ast) {
  assert(state != NULL);
  assert(ast != NULL);
}

int
main(int argc, char *argv[argc + 1]) {
  int return_value = EXIT_SUCCESS;
  char *buffers[argc - 1];
  void *memctx = talloc_autofree_context();

  if (argc < 2) {
    fprintf(stderr, "Error: No arguments given\n");
    return EXIT_FAILURE;
  }

  for (int i = 1; i < argc; i++) {
    const char *const filename = argv[i];
    struct lex_state state;
    struct tok tok;

    if ((buffers[i - 1] = file_read_all(memctx, filename)) == NULL) {
      return_value = EXIT_FAILURE;
      goto finish;
    }

    state = (struct lex_state) {
      .pos = (struct pos) {
        .filename = filename,
        .ptr = buffers[i - 1],
        .line = 1,
        .column = 1,
        .index = 0
      },
      .str = buffers[i - 1],
    };

    tok = (struct tok) {
      .pos = (struct pos) {
        .filename = filename,
        .ptr = NULL,
        .line = 1,
        .column = 1,
        .index = 0
      },
      .type = tok_type_none,
      .length = 0,
    };

    while (tok.type != tok_type_eof) {
      tok.type = tok_type_none;

      switch(lex(&state, &tok)) {
      case lex_err_string_eof:
        lex_print_err("Unexpected end of file during string lexification",
                      &state, &tok);
        return_value = EXIT_FAILURE;
        goto finish;
      case lex_err_string_eol:
        lex_print_err("Unexpected end of line during string lexification",
                      &state, &tok);
        return_value = EXIT_FAILURE;
        goto finish;
      case lex_err_symbol_eof:
        lex_print_err("Unexpected end of file during symbol lexification",
                      &state, &tok);
        return_value = EXIT_FAILURE;
        goto finish;
      case lex_err_symbol_eol:
        lex_print_err("Unexpected end of line during symbol lexification",
                      &state, &tok);
        return_value = EXIT_FAILURE;
        goto finish;
      case lex_err_symbol_space:
        lex_print_err("Unexpected spacing during symbol lexification",
                      &state, &tok);
        return_value = EXIT_FAILURE;
        goto finish;
      case lex_err_symbol_bracket:
        lex_print_err("Unexpected bracket during symbol lexification",
                      &state, &tok);
        return_value = EXIT_FAILURE;
        goto finish;
      case lex_err_none:
        break;
      }

      printf("%s:%lu:%lu (%lu) ",
             tok.pos.filename,
             (unsigned long) tok.pos.line,
             (unsigned long) tok.pos.column,
             (unsigned long) tok.length);

      switch(tok.type) {
      case tok_type_none:
        printf("None\n");
        break;
      case tok_type_atom:
        printf("Atom (%.*s)\n", (int) tok.length, tok.pos.ptr);
        break;
      case tok_type_string:
        printf("String (%.*s)\n", (int) tok.length, tok.pos.ptr);
        break;
      case tok_type_int:
        printf("Int (TODO)\n");
        break;
      case tok_type_float:
        printf("Float (TODO)\n");
        break;
      case tok_type_symbol:
        printf("Symbol (%.*s)\n", (int) tok.length, tok.pos.ptr);
        break;
      case tok_type_eof:
        printf("EOF\n");
        break;
      case tok_type_quote:
        printf("Quote\n");
        break;
      case tok_type_oparen:
        printf("OParen\n");
        break;
      case tok_type_cparen:
        printf("CParen\n");
        break;
      }
    }
  }

 finish:
  return return_value;
}

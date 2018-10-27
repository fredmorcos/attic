#include <stdio.h>
#include <inttypes.h>

enum ast_type {
  ast_type_module,
  ast_type_list,
  ast_type_id,
  ast_type_unsigned_int
};

struct token {
  size_t line;
  size_t col;
  size_t len;

  char *str;
  char *filename;
};

struct ast {
  enum ast_type type;
  struct token *token;

  union {
    struct {
      const char *name;
      struct ast *children;
    } module_info;

    struct {
      struct token *end_token;
      struct ast *children;
    } list_info;

    uint64_t unsigned_int;
    const char *id;
  };
};

int main(void) {
  struct ast module = {
    .type = ast_type_module,
    .module_info.name = "Main",
    .module_info.children = (struct ast []) {
      {
        .type = ast_type_list,
        .list_info.children = (struct ast []) {
          {
            .type = ast_type_id,
            .id = "+"
          },
          {
            .type = ast_type_unsigned_int,
            .unsigned_int = 5
          },
          {
            .type = ast_type_unsigned_int,
            .unsigned_int = 3
          }
        }
      },
      {
        .type = ast_type_list,
        .list_info.children = (struct ast []) {
          {
            .type = ast_type_id,
            .id = "+"
          },
          {
            .type = ast_type_unsigned_int,
            .unsigned_int = 8
          },
          {
            .type = ast_type_unsigned_int,
            .unsigned_int = 2
          }
        }
      }
    }
  };

  printf("%s\n", module.module_info.name);
  printf("%s %lu %lu\n",
         module.module_info.children[0].list_info.children[0].id,
         module.module_info.children[0].list_info.children[1].unsigned_int,
         module.module_info.children[0].list_info.children[2].unsigned_int);

  return 0;
}

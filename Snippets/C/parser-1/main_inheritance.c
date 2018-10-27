#include <stdio.h>
#include <inttypes.h>

struct token {
  size_t line;
  size_t col;
  size_t len;

  char *str;
  char *filename;
};

struct ast {
  struct token *token;
};

struct ast_compound {
  struct ast parent;
  struct ast *children;
};

struct ast_module {
  struct ast_compound parent;
  const char *name;
};

struct ast_list {
  struct ast_compound parent;
  struct token *end_token;
};

struct ast_unsigned_int {
  struct ast parent;
  uint64_t value;
};

struct ast_id {
  struct ast parent;
  char *id;
};

int main(void) {
  struct ast module = {
    .type = ast_type_module,
    .module_info = {
      .name = "Main",
      .children = (struct ast []) {
        {
          .type = ast_type_list,
          .list_children = (struct ast []) {
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
          .list_children = (struct ast []) {
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
    }
  };

  printf("%s\n", module.module_info.name);
  printf("%s %lu %lu\n",
         module.module_info.children[0].list_children[0].id,
         module.module_info.children[0].list_children[1].unsigned_int,
         module.module_info.children[0].list_children[2].unsigned_int);

  return 0;
}

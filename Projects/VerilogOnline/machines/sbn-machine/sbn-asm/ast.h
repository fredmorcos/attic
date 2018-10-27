#ifndef AST_H
#define AST_H

enum ast_type {
  PROGRAM = 400,
  DATA_SEC,
  SINGLE_DATA,
  TEXT_SEC,
  SINGLE_INSTRUCTION,
  INSTRUCTION,
  REFERENCE,
  NUMBER,
  SIGNED_NUMBER
};

typedef struct _ast
{
  int type, childnum, line;
  char *text;
  struct _ast **children;
} Ast;

Ast *result_ast;

Ast *ast_new(int, char *, int);
void ast_add_child(Ast *, Ast *);
void ast_free(Ast *);

#endif

#ifndef __AST_H__
#define __AST_H__

typedef struct _ast
{
  char *text;
  int type,
      lineno;
} Ast;

Ast *ast_new(char *, int, int);

#endif /* __AST_H__ */

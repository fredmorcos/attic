#ifndef __AST_H__
#define __AST_H__

#include "token.h"

typedef struct _ast
{
  struct _ast *parent,
              *rchild,
              *lchild;
  Token *token;
} Ast;

Ast *ast_new(Token *);

#endif /* __AST_H__ */


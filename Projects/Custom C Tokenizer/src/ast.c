#include "ast.h"
#include "extra.h"

#include <stdlib.h>
#include <stdio.h>

Ast *
ast_new(Token *token)
{
  Ast *tmp = (Ast *) malloc(sizeof(Ast));

  if (!tmp)
    extra_die("cannot allocate ast node");

  tmp->parent = NULL;
  tmp->rchild = NULL;
  tmp->lchild = NULL;
  tmp->token = token;

  return tmp;
}


#include "ast.h"
#include "parser.h"
#include <stdlib.h>

Ast *
ast_new(int type, char *text, int line)
{
  Ast *tmp = malloc(sizeof(Ast));

  tmp->type = type;
  tmp->childnum = 0;
  tmp->text = text;
  tmp->line = line;
  tmp->children = NULL;

  return tmp;
}

void
ast_add_child(Ast *ast, Ast *child)
{
  ast->childnum++;
  ast->children = realloc(ast->children, ast->childnum * sizeof(Ast *));
  ast->children[ast->childnum - 1] = child;
}

void
ast_free(Ast *ast)
{
  if(ast->text)
    free(ast->text);

  if(ast->children)
    {
      int i = 0;
      while (i < ast->childnum)
	{
	  ast_free(ast->children[i]);
	  i++;
	}

      free(ast->children);
    }

  free(ast);
}

#include "ast.h"
#include <stdlib.h>

Ast *
ast_new(char *_text, int _type, int _lineno)
{
  Ast *tmp = (Ast *) malloc(sizeof(Ast));
  tmp->text = _text;
  tmp->type = _type;
  tmp->lineno = _lineno;
  return tmp;
}

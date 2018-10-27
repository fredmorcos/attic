#include "token.h"
#include "extra.h"

#include <stdlib.h>

/* all known tokens, ie, keywords, operators, etc...
 * IMPORTANT: THESE LISTS HAVE TO STAY IN THE SAME ORDER AS THE 
 * 			  DEFINES IN token.h!!!
 */
const char *keywords[] = {"ret", "main", "print", NULL},
           *type_names[] = {"int", "void", NULL},
           *operators[] = {"=", "+", "-", "*", "/", NULL},
           *delimiters[] = {"{", "}", "(", ")", ",", NULL};

Token *token_new()
{
  Token *tmp = (Token *) malloc(sizeof(Token));

  if (!tmp)
    extra_die("cannot allocate token");

  tmp->text[0] = '\0';
  tmp->type = UNKNOWN;
  tmp->len = 0;
  tmp->lineno = 0;
  tmp->colno = 0;

  return tmp;
}

void token_text_add_char(Token *token, char c)
{
  if (token->len == TOKEN_TEXT_MAX_LEN)
    {
      TOKPRINTERR(token);
      extra_die("token too long");
    }

  token->text[token->len++] = c;
  token->text[token->len] = '\0';
}


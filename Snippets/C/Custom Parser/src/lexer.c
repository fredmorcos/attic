#include "lexer.h"
#include "token.h"
#include "extra.h"

#include <stdlib.h>
#include <ctype.h>

#define NEXTC ((c = fgetc(input)) != EOF)
#define TOKIN(x) ((tmp_index = extra_str_in(token->text, x)) != -1)

static Token *lookahead = NULL;

Token *lexer_next_token_peek(FILE *input)
{
  if (lookahead)
    return lookahead;

  lookahead = lexer_next_token(input);
  return lookahead;
}

Token *lexer_next_token(FILE *input)
{
  static unsigned int lineno = 1,
                      colno = 0;

  char c;
  int tmp_index = 0;
  Token *token = NULL;
	
  if (lookahead)
    {
      token = lookahead;
      lookahead = NULL;
      goto finish_return;
    }

  token = token_new();

  while NEXTC
    {
      ++colno;

      /* whitespace */
      if (isspace(c))
	{
	  if (c == '\n')
	    {
	      ++lineno;
	      colno = 1;
	    }

	  continue;
	}

      token_text_add_char(token, c);
      token->colno = colno;

      if (isalpha(c))
	{
	  while NEXTC
	    {
	      if (!(isalnum(c) || c == '_'))
		goto finish_alnum_word;
				
	      ++colno;
	      token_text_add_char(token, c);
	    }

	finish_alnum_word:
	  ungetc(c, input);

	  if TOKIN(keywords)
	    token->type = _KEYWORD_GROUP + tmp_index;
	  else if TOKIN(type_names)
	    token->type = TYPE_NAME;
	  else
	    token->type = IDENTIFIER;

	  goto finish;
	}
      else if (isdigit(c))
	{
	  token->type = LITERAL_INTEGER;
			
	  while NEXTC
	    {
	      if (!isdigit(c))
		{
		  ungetc(c, input);
		  goto finish;
		}

	      ++colno;
	      token_text_add_char(token, c);
	    }

	  goto finish;
	}
      else
	{
	  if TOKIN(operators)
	    token->type = _OPERATOR_GROUP + tmp_index;
	  else if TOKIN(delimiters)
	    token->type = _DELIMITER_GROUP + tmp_index;
	  else
	    {
	      TOKPRINTERR(token);
	      extra_die("invalid token");
	    }

	  goto finish;
	}
    }

 finish:
  if (c == EOF)
    {
      free(token);
      return NULL;
    }

  token->lineno = lineno;

 finish_return:
  return token;
}


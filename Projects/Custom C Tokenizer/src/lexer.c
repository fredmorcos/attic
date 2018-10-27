#include "lexer.h"
#include "token.h"
#include "extra.h"

#include <stdlib.h>
#include <ctype.h>

#define GETC   ((c = fgetc(input)) != EOF)
#define UNGETC (ungetc(c, input))
#define INCLIN (if (c == '\n') { ++lineno; colno = 1; })
#define TOKPUT (++colno; token_text_add_char(token, c);)

Token *
lexer_next_token(FILE *input)
{
  static unsigned int lineno = 1,
                      colno = 0;

  char c;
  Token *token = NULL;
	
  token = token_new();

  while GETC
    {
      ++colno;

      /* whitespace */
      if (isspace(c))
	{
	  INCLIN

	  while GETC
	    {
	      if (isspace(c)) INCLIN;
	      else goto f_ws;

	      TOKPUT;
	    }

	f_ws:
	  UNGETC;
	}

      TOKPUT;
      token->colno = colno;

      if (isalpha(c))
	{
	  while GETC
	    {
	      if (!(isalnum(c) || c == '_')) goto f_alnum;
	      TOKPUT;
	    }

	f_alnum:
	  UNGETC;
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


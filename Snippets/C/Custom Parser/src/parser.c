#include "parser.h"
#include "token.h"
#include "lexer.h"
#include "extra.h"

#include <stdlib.h>

#define INVALID         0
#define VALID           1
#define FINISHED        2

int parser_match_program(FILE *);
int parser_match_functions(FILE *);
int parser_match_function(FILE *);
int parser_match_statements(FILE *);
int parser_match_arguments(FILE *);
int parser_match(FILE *, int, int, Token *);
int parser_match_argument(FILE *);
int parser_match_declaration(FILE *);

Ast *
parser_build_ast(FILE *input)
{
  Ast *ast = ast_new(NULL);

  if (parser_match_program(input) == INVALID)
    extra_die("improperly formed program");

  return ast;
}

int
parser_match_program(FILE *input)
{
  return parser_match_functions(input);
}

int
parser_match_functions(FILE *input)
{
  int res;

  while ((res = parser_match_function(input)) == VALID)
    ;

  return res;
}

int
parser_match_function(FILE *input)
{
  Token *token = NULL;

  if (parser_match(input, TYPE_NAME, 0, token) == VALID)
    {
    }
  else
    return INVALID;

  if (parser_match(input, IDENTIFIER, 0, token) == VALID)
    {
    }
  else
    return INVALID;

  if (parser_match(input, DELIMITER_LPAREN, 0, token) == VALID)
    {
    }
  else
    return INVALID;

  parser_match_arguments(input);

  if (parser_match(input, DELIMITER_RPAREN, 0, token) == VALID)
    {
    }
  else
    return INVALID;

  if (parser_match(input, DELIMITER_LBRACE, 0, token) == VALID)
    {
    }
  else
    return INVALID;

  parser_match_statements(input);

  if (parser_match(input, DELIMITER_RBRACE, 0, token) == VALID)
    {
    }
  else
    return INVALID;
	
  return VALID;
}

int
parser_match_arguments(FILE *input)
{
  Token *token = NULL;

  while (1)
    {
      if (parser_match_argument(input) == VALID)
	{
	}
      else
	return INVALID;
      
      if (lexer_next_token_peek(input)->type != DELIMITER_COMMA)
	return VALID;
      
      if (parser_match(input, DELIMITER_COMMA, 0, token) == VALID)
	{
	}
      else
	return INVALID;
    }

  return VALID;
}

int
parser_match_argument(FILE *input)
{
  Token *token = NULL;

  if (lexer_next_token_peek(input)->type != TYPE_NAME)
    return VALID;

  if (parser_match(input, TYPE_NAME, 0, token) == VALID)
    {
    }
  else
    return INVALID;

  if (parser_match(input, IDENTIFIER, 0, token) == VALID)
    {
    }
  else
    return INVALID;

  return VALID;
}

int
parser_match_statements(FILE *input)
{
  while (1)
    {
      if (lexer_next_token_peek(input)->type == TYPE_NAME)
	parser_match_declaration(input);
      else if (lexer_next_token_peek(input)->type == IDENTIFIER)
	parser_match_assignment(input);
      else
	break;
    }

  return VALID;
}

int
parser_match_assignment(FILE *input)
{
  Token *token = NULL;

  if (parser_match(input, IDENTIFIER, 0, token) == VALID)
    {
    }
  else
    return INVALID;

  if (parser_match(input, OPERATOR_ASSIGN, 0, token) == VALID)
    {
    }
  else
    return INVALID;

  return parser_match_expression(input);
}

int
parser_match_expression(FILE *input)
{
  Token *token = NULL;

  
}

int
parser_match_declaration(FILE *input)
{
  Token *token = NULL;

  if (parser_match(input, TYPE_NAME, 0, token) == VALID)
    {
    }
  else
    return INVALID;

  if (parser_match(input, IDENTIFIER, 0, token) == VALID)
    {
    }
  else
    return INVALID;

  return VALID;
}

int
parser_match(FILE *input, int token_type, int optional, Token *token)
{
  Token *tmp = lexer_next_token_peek(input);

  if (!tmp)
    return FINISHED;

  TOKPRINT(tmp);

  if (tmp->type == token_type)
    {
      token = lexer_next_token(input);
      return VALID;
    }
	
  if (optional)
    {
      token = NULL;
      return VALID;
    }

  TOKPRINTERR(tmp);
  extra_die("improperly formed program, unexpected token");

  /* that's just for gcc */
  return INVALID;
}


#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#define OPEN_BRACKET 0
#define CLOSE_BRACKET 1
#define OP_ADD 2
#define OP_SUB 3
#define TOK 4

static FILE *in = NULL;
static char token[256];

int
main (int argc, char **argv)
{
  int lex_res = -1;

  in = fopen(argv[argc - 1], "r");

  while ((lex_res = lex()) != -1)
    {
      switch (lex_res)
	{
	case OPEN_BRACKET:
	  printf("ob\n");
	  break;
	case CLOSE_BRACKET:
	  printf("cb\n");
	  break;
	case OP_ADD:
	  printf("add\n");
	  break;
	case OP_SUB:
	  printf("sub\n");
	  break;
	case TOK:
	  printf("%s\n", token);
	  break;
	default:
	  printf("finish\n");
	  break;
	}
    }

  fclose(in);
  return EXIT_SUCCESS;
}

int
lex ()
{
  int token_p = 0;
  char c;

  while ((c = fgetc(in)) != EOF)
    {
      if (c == '(')
	return OPEN_BRACKET;

      if (c == ')')
	return CLOSE_BRACKET;

      if (c == '+')
	return OP_ADD;

      if (c == '-')
	return OP_SUB;

      if (isspace(c))
	while (isspace(c = fgetc(in)))
	  ;

      token[token_p++] = c;
      
      while (!isspace(c = fgetc(in)) && isdigit(c))
	{
	  token[token_p++] = c;
	  
	  if (token_p > 255)
	    exit(EXIT_FAILURE);
	}

      token[token_p] = '\0';
      return TOK;
    }

  return -1;
}

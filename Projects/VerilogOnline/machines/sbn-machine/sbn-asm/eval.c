#include "eval.h"
#include "parser.h"
#include "symtab.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ISTERMINAL(x) (x->type < 400 ? 1 : 0)
#define RECURSE() do {				\
    for (i = 0; i < ast->childnum; i++)		\
      {						\
	res = __eval_ast(ast->children[i]);	\
	if (res)				\
	  return res;				\
      }						\
  } while (0);
#define RESET() do {				\
    mode = 0;					\
    program_addr = 0;				\
    data_addr = 0;				\
  } while (0);
#define ERROROUT(msg) do {					\
    if (ast->line == -1)					\
      fprintf(stderr, "error: %s:%d in program, message: %s\n",	\
	      __FUNCTION__, __LINE__, msg);			\
    else							\
      fprintf(stderr, "error: %s:%d at line %d, message: %s\n",	\
	      __FUNCTION__, __LINE__, ast->line, msg);		\
    return 1;							\
  } while (0);
#define STDOUTPRINT(data) do {			\
    if (!literal_mode) printf(data);		\
  } while (0);
#define STDOUTPRINTFMT(fmt, data) do {		\
    if (!literal_mode) printf(fmt, data);	\
  } while (0);
#define FORMATSTR(mode) (mode ? "%02x" : "%x")

int __eval_ast(Ast *);

extern FILE *program_output,
  *data_output,
  *literal_program_output,
  *literal_data_output;
extern SymTab *program_table,
  *data_table;

static int program_addr = 0,
  data_addr = 0,
  mode = 0,                /* 0: data, 1: text */
  literal_mode = 0,
  symbol_table_mode = 0;   /* 0: no, 1: yes */
static FILE *out = 0;

int
eval_ast(Ast *ast)
{
  int res = 0;
  
  /* pass 1: build symbol table */
  symbol_table_mode = 1;
  RESET();

  res = __eval_ast(ast);
  if (res)
    return res;

  /* pass 2: compile source-to-source */
  symbol_table_mode = 0;
  RESET();

  res = __eval_ast(ast);
  if (res)
    return res;

  /* pass 3: rewrite original source to add "debug" info */
  symbol_table_mode = 0;
  RESET();
  literal_mode = 1;

  res = __eval_ast(ast);
  return res;
}

int
__eval_ast(Ast *ast)
{
  int i = 0,
    res = 0;
  unsigned int tmp;

  if (literal_mode)
    {
      if (ISTERMINAL(ast))
	{
	  if (mode == 0 && ast->type == COMMA)
	    {
	      fprintf(out, "\n");
	      STDOUTPRINT("\n");
	      return 0;
	    }

	  if (ast->type == DATA_DIR)
	    {
	      mode = 0;
	      out = literal_data_output;
	      goto finish_bypass;
	    }
	  else if (ast->type == TEXT_DIR)
	    {
	      mode = 1;
	      out = literal_program_output;
	      goto finish_bypass;
	    }
	  else
	    {
	      fprintf(out, "%s", ast->text);
	      STDOUTPRINTFMT("%s", ast->text);
	    }
	}
      else
	{
	  RECURSE();

	  if (ast->type == SINGLE_INSTRUCTION)
	    {
	      fprintf(out, "\n");
	      STDOUTPRINT("\n");
	    }
	}

      return 0;
    }

  if (symbol_table_mode)
    {
      switch (ast->type)
	{
	case SINGLE_DATA:
	  RECURSE();
	  goto finish;
	  break;

	case SINGLE_INSTRUCTION:
	  RECURSE();
	  goto finish;
	  break;

	case LABEL:
	  if (mode == 0)
	    i = symtab_add_node(data_table, data_addr, ast->text);
	  else if (mode == 1)
	    i = symtab_add_node(program_table, program_addr, ast->text);
	  else
	    ERROROUT("unknown mode");

	  if (i == -1)
	    ERROROUT("label already in symbol table");
	  break;

	case DATA_DIR:
	  mode = 0;
	  out = data_output;
	  break;

	case TEXT_DIR:
	  mode = 1;
	  out = program_output;
	  break;

	default:
	  RECURSE();
	  break;
	}
      goto finish_bypass;
    }

  switch (ast->type)
    {
    case DEC:
      tmp = (unsigned int) strtol(ast->text, NULL, 10);
      fprintf(out, FORMATSTR(mode), tmp);
      STDOUTPRINTFMT(FORMATSTR(mode), tmp);
      break;

    case HEX:
      tmp = (unsigned int) strtol(ast->text, NULL, 16);
      fprintf(out, FORMATSTR(mode), tmp);
      STDOUTPRINTFMT(FORMATSTR(mode), tmp);
      break;

    case BIN:
      tmp = (unsigned int) strtol(ast->text + 2, NULL, 2);
      fprintf(out, FORMATSTR(mode), tmp);
      STDOUTPRINTFMT(FORMATSTR(mode), tmp);
      break;

    case ID:
      if (mode == 0)
	{
	  i = symtab_get_val(data_table, ast->text);
	  if (i == -1)
	    i = symtab_get_val(program_table, ast->text);
	}
      else if (mode == 1)
	{
	  i = symtab_get_val(program_table, ast->text);
	  if (i == -1)
	    i = symtab_get_val(data_table, ast->text);
	}
      else
	ERROROUT("unknown mode");

      if (i == -1)
	{
	  fprintf(stderr, "mode %d, label: %s\n", mode, ast->text);
	  ERROROUT("label not in symbol table");
	}

      fprintf(out, FORMATSTR(mode), i);
      STDOUTPRINTFMT(FORMATSTR(mode), i);
      break;

    case LABEL:
      /* already handled in symbol table mode */
      break;

    case DATA_DIR:
      mode = 0;
      out = data_output;
      STDOUTPRINT("\n\n=== DATA ===\n\n");
      break;

    case TEXT_DIR:
      mode = 1;
      out = program_output;
      STDOUTPRINT("\n\n=== TEXT ===\n\n");
      break;

    case COMMA:
      if (mode == 0)
	{
	  fprintf(out, "\n");
	  STDOUTPRINT("\n");
	}
      else if (mode == 1)
	{
	  fprintf(out, "_");
	  STDOUTPRINT("_");
	}
      else
	ERROROUT("unknown mode");
      break;

    case SIGNED_NUMBER:
      if (ast->children[0]->type == MINUS)
	{
	  Ast *number = ast->children[1]->children[0];
	  unsigned int inumber = 0;

	  switch (number->type)
	    {
	    case DEC:
	      inumber = strtol(number->text, NULL, 10);
	      break;

	    case HEX:
	      inumber = strtol(number->text, NULL, 16);
	      break;

	    case BIN:
	      inumber = strtol(number->text + 2, NULL, 2);
	      break;

	    default:
	      ERROROUT("unknown number type");
	    }

	  inumber = ~inumber;
	  inumber++;
	  fprintf(out, FORMATSTR(mode), inumber);
	  STDOUTPRINTFMT(FORMATSTR(mode), inumber);
	}
      else
	RECURSE();
      break;

    case MINUS:
      ERROROUT("shouldn't be here");
      break;

    case SINGLE_INSTRUCTION:
      RECURSE();
      fprintf(out, "\n");
      STDOUTPRINT("\n");
      break;

    default:
      RECURSE();
    }

 finish:
  /* after everything is finished, increment the addressing counters */
  if (mode == 0)
    data_addr++;
  else if (mode == 1)
    program_addr++;
  else
    ERROROUT("unknown mode");

 finish_bypass:
  return res;
}

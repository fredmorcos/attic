#include <stdio.h>
#include <stdlib.h>
#include "eval.h"
#include "ast.h"
#include "symtab.h"

extern int yyparse();
extern FILE *program_output,
  *data_output,
  *literal_program_output,
  *literal_data_output,
  *yyin;

extern Ast *result_ast;
extern SymTab *program_table,
  *data_table;

int
main (int argc, char **argv)
{
  int res;

  if (argc < 5)
    {
      fprintf(stderr, "usage: sbnasm <input-file>\n");
      fprintf(stderr, "              <prog-out> <data-out>\n");
      fprintf(stderr, "              <dbg-prog-out> <dbg-data-out>\n");
      exit(1);
    }

  /* bison and flex's input */
  yyin = fopen(argv[1], "r");

  /* output from bison parsing */
  program_output = fopen(argv[2], "w");
  data_output = fopen(argv[3], "w");
  literal_program_output = fopen(argv[4], "w");
  literal_data_output = fopen(argv[5], "w");

  /* run parser and eval */
  res = yyparse();

  if (res)
    {
      fprintf(stderr, "error: parser (code %d)\n", res);
      goto finish;
    }

  program_table = symtab_new();
  data_table = symtab_new();
  res = eval_ast(result_ast);

  if (res)
    {
      fprintf(stderr, "error: eval (code %d)\n", res);
      goto finish;
    }

 finish:
  /* close in and out files */
  fclose(yyin);
  fclose(program_output);
  fclose(data_output);
  fclose(literal_program_output);
  fclose(literal_data_output);

  ast_free(result_ast);
  symtab_free(program_table);
  symtab_free(data_table);

  return res;
}

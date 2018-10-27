%{
#include <stdio.h>
#include <stdlib.h>
#include "ast.h"

FILE *program_output,
  *data_output,
  *literal_program_output,
  *literal_data_output;

void yyerror(const char *);

extern int yylex();
extern int yylineno;
extern char *yytext;
extern Ast *result_ast;
%}

%union { struct _ast *ast; }

%token <ast> DEC HEX BIN ID LABEL DATA_DIR TEXT_DIR COMMA MINUS
%type  <ast> program data_sec single_data text_sec single_instruction
%type  <ast> instruction reference number signed_number

%%

program: DATA_DIR data_sec TEXT_DIR text_sec {
  Ast *tmp = ast_new(PROGRAM, NULL, -1);
  ast_add_child(tmp, $1);
  ast_add_child(tmp, $2);
  ast_add_child(tmp, $3);
  ast_add_child(tmp, $4);
  result_ast = tmp;
  $$ = tmp;
};

data_sec:
single_data {
  Ast *tmp = ast_new(DATA_SEC, NULL, -1);
  ast_add_child(tmp, $1);
  $$ = tmp;
} |
single_data COMMA data_sec {
  Ast *tmp = ast_new(DATA_SEC, NULL, -1);
  ast_add_child(tmp, $1);
  ast_add_child(tmp, $2);
  ast_add_child(tmp, $3);
  $$ = tmp;
};

single_data:
signed_number {
  Ast *tmp = ast_new(SINGLE_DATA, NULL, -1);
  ast_add_child(tmp, $1);
  $$ = tmp;
} |
LABEL signed_number {
  Ast *tmp = ast_new(SINGLE_DATA, NULL, -1);
  ast_add_child(tmp, $1);
  ast_add_child(tmp, $2);
  $$ = tmp;
};

text_sec:
single_instruction {
  Ast *tmp = ast_new(TEXT_SEC, NULL, -1);
  ast_add_child(tmp, $1);
  $$ = tmp;
} |
single_instruction text_sec {
  Ast *tmp = ast_new(TEXT_SEC, NULL, -1);
  ast_add_child(tmp, $1);
  ast_add_child(tmp, $2);
  $$ = tmp;
};

single_instruction:
instruction {
  Ast *tmp = ast_new(SINGLE_INSTRUCTION, NULL, -1);
  ast_add_child(tmp, $1);
  $$ = tmp;
} |
LABEL instruction {
  Ast *tmp = ast_new(SINGLE_INSTRUCTION, NULL, -1);
  ast_add_child(tmp, $1);
  ast_add_child(tmp, $2);
  $$ = tmp;
};

instruction:
reference COMMA reference COMMA reference COMMA reference {
  Ast *tmp = ast_new(INSTRUCTION, NULL, -1);
  ast_add_child(tmp, $1);
  ast_add_child(tmp, $2);
  ast_add_child(tmp, $3);
  ast_add_child(tmp, $4);
  ast_add_child(tmp, $5);
  ast_add_child(tmp, $6);
  ast_add_child(tmp, $7);
  $$ = tmp;
};

reference:
number {
  Ast *tmp = ast_new(REFERENCE, NULL, -1);
  ast_add_child(tmp, $1);
  $$ = tmp;
} |
ID {
  Ast *tmp = ast_new(REFERENCE, NULL, -1);
  ast_add_child(tmp, $1);
  $$ = tmp;
};

number:
DEC {
  Ast *tmp = ast_new(NUMBER, NULL, -1);
  ast_add_child(tmp, $1);
  $$ = tmp;
} |
HEX {
  Ast *tmp = ast_new(NUMBER, NULL, -1);
  ast_add_child(tmp, $1);
  $$ = tmp;
} |
BIN {
  Ast *tmp = ast_new(NUMBER, NULL, -1);
  ast_add_child(tmp, $1);
  $$ = tmp;
};

signed_number:
MINUS number {
  Ast *tmp = ast_new(SIGNED_NUMBER, NULL, -1);
  ast_add_child(tmp, $1);
  ast_add_child(tmp, $2);
  $$ = tmp;
} |
number {
  Ast *tmp = ast_new(SIGNED_NUMBER, NULL, -1);
  ast_add_child(tmp, $1);
  $$ = tmp;
};

%%

void
yyerror(const char *str) {
  fprintf(stderr, "error at line %d: parser - %s (token %s)\n", yylineno, str, yytext);
  exit(1);
}

%{
#include <stdio.h>
#include "ast.h"
%}

%token ID
%token OP_ADD OP_ASSIGN
%token KW_RET KW_PRINT KW_MAIN KW_END
%token LIT_INT
%token DEL_COMMA DEL_EOL DEL_LPAREN DEL_RPAREN

%%

program:
definitions
;

definitions:
definition
| definition DEL_EOL definitions
;

definition:
functions
;

functions:
function
| function functions
;

function:
KW_MAIN function_body
| ID DEL_LPAREN arguments DEL_RPAREN function_body
;

function_body:
DEL_EOL statements KW_END DEL_EOL
;

arguments:
ID
| ID DEL_COMMA arguments
;

statements:
statement DEL_EOL
| statement DEL_EOL statements
;

statement:
/* nothing */
| assignment
| funcall
| KW_RET expression
| KW_PRINT expression
;

assignment:
ID OP_ASSIGN expression
| ID OP_ASSIGN funcall
;

funcall:
ID DEL_LPAREN parameters DEL_RPAREN
;

parameters:
expression
| expression DEL_COMMA parameters
;

expression:
rvalue
| rvalue OP_ADD expression
;

rvalue:
ID
| LIT_INT
;

%%

int
yyerror (char *error)
{
  fprintf(stderr, "parser error: %s\n", error);
}

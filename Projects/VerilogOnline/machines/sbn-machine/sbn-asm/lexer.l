%option noyywrap nounput noinput yylineno

%{
#include <stdio.h>
#include <string.h>
#include "parser.h"
#include "ast.h"

extern void yyerror(const char *);

#define ADVANCE(type) do {				        \
yylval.ast = ast_new(type, strdup(yytext), yylineno);		\
return type;				                 	\
} while (0);
%}

DEC         [0-9]+
HEX         0x[0-9A-Fa-f]+
BIN         0b[0-1]+

ID          [A-Za-z_]+[A-Za-z0-9_]*
LABEL       [A-Za-z_]+[A-Za-z0-9_]*\:

DATA_DIR    .data
TEXT_DIR    .text

COMMA       ","
MINUS       "-"

WS          [ \t]+
NL          [\n]+
HASH        \#

%x COMMENT

%%

{DEC}                { ADVANCE(DEC); }
{HEX}                { ADVANCE(HEX); }
{BIN}                { ADVANCE(BIN); }

{ID}                 { ADVANCE(ID);    }
{LABEL}              { ADVANCE(LABEL); }

{DATA_DIR}           { ADVANCE(DATA_DIR); }
{TEXT_DIR}           { ADVANCE(TEXT_DIR); }

{COMMA}              { ADVANCE(COMMA); }
{MINUS}              { ADVANCE(MINUS); }

{WS}                 { }
{NL}                 { }
{HASH}               { BEGIN(COMMENT); }

<COMMENT>{NL}        { BEGIN(INITIAL); }

<INITIAL,COMMENT>.   { }

%%


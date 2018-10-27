%option noyywrap yylineno

%{

#include <stdio.h>
#include "par.tab.h"

int yylval;

%}

%%

"+"     { return ADD; }
[0-9]+  { yylval = atoi(yytext) + 1; return NUM; }
[\n]+   { return EOL; }
[ \t]+  { }
.       { puts("flex: unrecognized"); }

%%

%option noyywrap yylineno nodefault

%{
#include <stdio.h>
#include <stdlib.h>
#include "par.tab.h"
#include "extra.h"
#include "ast.h"
%}

%%

"+"                     { PRINT_INFO_LIT(OP_ADD);     return OP_ADD; }
"="                     { PRINT_INFO_LIT(OP_ASSIGN);  return OP_ASSIGN; }
"("                     { PRINT_INFO_LIT(DEL_LPAREN); return DEL_LPAREN; }
")"                     { PRINT_INFO_LIT(DEL_RPAREN); return DEL_RPAREN; }
","                     { PRINT_INFO_LIT(DEL_COMMA);  return DEL_COMMA; }
[\n]+                   { PRINT_INFO_LIT(DEL_EOL);    return DEL_EOL; }
"print"                 { PRINT_INFO_LIT(KW_PRINT);   return KW_PRINT; }
"main"                  { PRINT_INFO_LIT(KW_MAIN);    return KW_MAIN; }
"ret"                   { PRINT_INFO_LIT(KW_RET);     return KW_RET; }
"end"                   { PRINT_INFO_LIT(KW_END);     return KW_END; }
[0-9]+                  { PRINT_INFO_LIT(LIT_INT);    return LIT_INT; }
[a-zA-Z_][a-zA-Z_0-9]*  { PRINT_INFO_LIT(ID);         return ID; }
[ \t]+                  { /* skip whitespace */ }
.       {
          printf("%d: lexer error: unrecognized token: %s\n", yylineno, yytext);
          exit(EXIT_FAILURE);
        }

%%


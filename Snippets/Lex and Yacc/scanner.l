#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void yyerror(char *s);
int lineno = 1;

%}

ws        [ \t]+
comment   ;.*
id        [_a-zA-Z][_a-zA-Z0-9\.\-]*
intnum    -?[0-9]+
hexnum    0x[0-9a-fA-F]+

%%

{ws}      ; /* ignore */
{comment} ; /* ignore */

[:()]     { return yytext[0]; }

{id}      {
             int optype = is_op(yytext);
             yylval.id = strdup(yytext);
	     if (optype >= 0)
	       return optype;
	     else
	       return ID;
          }

{intnum}  {
             yylval.intnum = atoi(yytext);
             return INTNUM;
          }

{hexnum}  {
             if (sscanf(yytext+2, "%x", &yylval.intnum) != 1)
               yyerror("invalid hexnum");
             return INTNUM;
          }

{reg}     {
             yylval.reg = reg_name_to_num(yytext+1/* skip $ */);
             return REG;
          }

\n        { lineno++;  return yytext[0]; }
.         { return yytext[0]; }

%%

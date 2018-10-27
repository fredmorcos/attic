%{
#include <stdio.h>
%}

%token NUM ADD EOL

%%

line:
/* nothing */
| expr EOL { printf("= %d\n", $1); }
;

expr: 
NUM { $$ = $1; }
| expr ADD expr { $$ = $1 + $3; }
;

%%

int yyerror (char *error)
{
  fprintf(stderr, "error: %s\n", error);
}

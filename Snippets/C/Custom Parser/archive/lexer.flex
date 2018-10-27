%{
    /* put literal C code here (ie, #includes) */
%}

/* token definitions */
DIGIT       [0-9]
INT         {DIGIT}+
FLOAT       {DIGIT}+["."{DIGIT}+]?
BOOL        true|false
ID          [a-zA-Z_]+[a-zA-Z_0-9]*
DELIM       ":"|","|"("|")"
OPS         "+"|"-"|"*"|"/"|"="
WS          [ \t\f\n\r]+
KW          func|main|ret|null|import|module
TYPE        int|float|char|bool|none

%%

{INT}       {   fprintf(stdout, "Integer: %s\n", yytext);
            }

{FLOAT}     {   fprintf(stdout, "Float: %s\n", yytext);
            }

{BOOL}      {   fprintf(stdout, "Bool: %s\n", yytext);
            }

{KW}        {   fprintf(stdout, "Keyword: %s\n", yytext);
            }

{TYPE}      {   fprintf(stdout, "Type: %s\n", yytext);
            }

{ID}        {   fprintf(stdout, "Identifier: %s\n", yytext);
            }

{DELIM}     {   fprintf(stdout, "Delimiter: %s\n", yytext);
            }

{OPS}       {   fprintf( stdout, "Operator: %s\n", yytext );
            }

{WS}        {
            }

.           {
                fprintf(stdout, "Unrecognized character: %s\n", yytext);
            }

<<EOF>>     {
                fprintf(stdout, "=== End of file ===\n");
                yyterminate();
            }

%%

%{
	#include <stdio.h>

	extern int yylex();
	void yyerror(const char *s) { fprintf(stderr, "parser error: %s\n", s); }
%}

%union
{
	char *string;
	int token;
}

%token <string> LITERAL_INTEGER IDENTIFIER TYPE_NAME
%token <token> KEYWORD_RETURN KEYWORD_MAIN KEYWORD_PRINT
%token <token> DELIMITER_LBRACE DELIMITER_RBRACE DELIMITER_COMMA DELIMITER_LPAREN DELIMITER_RPAREN
%token <token> OPERATOR_PLUS OPERATOR_MINUS OPERATOR_TIMES OPERATOR_DIVIDE OPERATOR_ASSIGN

%left OPERATOR_PLUS OPERATOR_MINUS
%left OPERATOR_TIMES OPERATOR_DIVIDE

%start program

%%

program:		/* empty */
	   |		funcs;
funcs:			func funcs;
func:			TYPE ID DELIM args DELIM DELIM NL statements
	|			TYPE KW DELIM args DELIM DELIM NL statements;
args:			arg DELIM args;
arg:			TYPE ID;
statements:		statement NL statements;
statement:		declaration
		 |		assignment
		 |		return;
declaration:	TYPE ID;
assignment:		ID OP expression;
return:			KW ID;
expression:		INTEGER OP INTEGER
		  		{
					printf("expression! %s %s %s\n", $1, $2, $3);
				}
		  |		INTEGER OP ID
		  |		ID OP ID
		  |		ID OP INTEGER
		  |		ID
		  |		INTEGER;

%%


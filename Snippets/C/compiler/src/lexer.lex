%{
	#include "symbols.h"

	symbol_t yysymbol;
%}

DIGIT							[0-9]
ALPHA							[a-zA-Z_]
INTEGER							{DIGIT}*
FLOAT							{DIGIT}+"."{DIGIT}*
IDENTIFIER						{ALPHA}+[{ALPHA}|{DIGIT}]*
KEYWORD_MAIN					main
WHITESPACE						[ \t\n]+
OPERATOR_UNARY					"++"|"--"
OPERATOR_BINARY					"+"|"-"|"*"|"/"|"%"|"="

LEFT_CURLY_BRACKET				\{
RIGHT_CURLY_BRACKET				\}
LEFT_BRACKET					\(
RIGHT_BRACKET					\)

%%

{INTEGER}					{ yysymbol = LITERAL_INTEGER;		return;	}
{FLOAT}						{ yysymbol = LITERAL_FLOAT;			return;	}
{LEFT_CURLY_BRACKET}		{ yysymbol = LEFT_CURLY_BRACKET;	return; }
{RIGHT_CURLY_BRACKET}		{ yysymbol = RIGHT_CURLY_BRACKET;	return; }
{LEFT_BRACKET}				{ yysymbol = LEFT_BRACKET;			return; }
{RIGHT_BRACKET}				{ yysymbol = RIGHT_BRACKET;			return; }
{KEYWORD_MAIN}				{ yysymbol = KEYWORD_MAIN;			return;	}
{IDENTIFIER}				{ yysymbol = IDENTIFIER;			return;	}
{OPERATOR_UNARY}			{ yysymbol = OPERATOR_UNARY;		return;	}
{OPERATOR_BINARY}			{ yysymbol = OPERATOR_BINARY;		return;	}
{WHITESPACE}				{ 											}
.							{ printf("Unknown: %s\n", yytext);			}

%%


%{
	#include "symbols.h"

	symbol_t yysymbol;
%}

DIGIT							[0-9]
ALPHA							[a-zA-Z_]
INTEGER							{DIGIT}*
FLOAT							{DIGIT}+"."{DIGIT}*
WHITESPACE						[ \t\n]+
DOUBLE_QUOTE					\"
PERCENT							"%"
EQUAL							"="

%%

<INITIAL>{INTEGER}					{ yysymbol = LIT_INTEGER;		return;	}
<INITIAL>{FLOAT}					{ yysymbol = LIT_FLOAT;			return;	}
<INITIAL>{WHITESPACE}				{ 										}
<INITIAL>.							{ printf("Unknown: %s\n", yytext);		}

%%


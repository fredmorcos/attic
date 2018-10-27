#include "lexer.h"
#include "verbose.h"
#include "token.h"
#include "symbols.h"
#include "lex.yy.h"
#include <stddef.h>
#include <assert.h>
#include <string.h>

extern symbol_t yysymbol;

Token *lexer_next_token(FILE *stream) {
	assert(stream != NULL);
	msg("Lexer");

	yyin = stream;
	yylex();

	if(yytext[0] == '\0')
		return NULL;
	return token_new(yysymbol, yytext);
}


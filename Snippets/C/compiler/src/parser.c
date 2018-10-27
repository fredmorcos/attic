#include "parser.h"
#include "lexer.h"
#include "verbose.h"
#include "symbols.h"
#include <stddef.h>
#include <assert.h>
#include <stdio.h>

void parser_parse(FILE *stream) {
	Token *token = NULL;

	assert(stream != NULL);
	msg("Parser");

	while((token = lexer_next_token(stream))) {
		if(!token)
			break;
		printf("Parser: got a %d with value %s.\n",
				token->type, token->value);
	}
}


#include "token.h"
#include <assert.h>
#include <stddef.h>
#include <stdlib.h>

Token *token_new(symbol_t type, const char *value) {
	Token *token = malloc(sizeof(Token));
	token->type = type;
	token->value = (char *)value;
	return token;
}


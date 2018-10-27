#ifndef __TOKEN_H__
#define __TOKEN_H__

#include "symbols.h"

struct _Token {
	symbol_t  type;
	char	 *value;
};

typedef struct _Token Token;

Token *token_new(symbol_t type, const char *value);

#endif /* __TOKEN_H__ */


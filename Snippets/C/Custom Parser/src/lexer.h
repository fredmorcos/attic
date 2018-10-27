#ifndef __LEXER_H__
#define __LEXER_H__

#include <stdio.h>
#include "token.h"

Token *lexer_next_token(FILE *);
Token *lexer_next_token_peek(FILE *);

#endif /* __LEXER_H__ */


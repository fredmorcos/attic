#ifndef __LEXER_H__
#define __LEXER_H__

#include "token.h"
#include <stdio.h>

Token* lexer_next_token(FILE *stream);

#endif /* __LEXER_H__ */


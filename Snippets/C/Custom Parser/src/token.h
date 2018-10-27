#ifndef __TOKEN_H__
#define __TOKEN_H__

#include <stdio.h>

/* TODO need to add filename to token output when multi-file support is in */
#define _TOKPRINT(x, s) (fprintf(s, "%d:%d-%d:%d: %s\n", x->lineno,	\
				 x->colno, x->colno + x->len, x->type, x->text))
#define TOKPRINT(x) _TOKPRINT(x, stdout)
#define TOKPRINTERR(x) _TOKPRINT(x, stderr)

extern const char *keywords[],
                  *type_names[],
                  *operators[],
                  *delimiters[];

/* token types */
#define UNKNOWN                 0
#define WHITESPACE              1

#define _LITERAL_GROUP          10
#define LITERAL_DIGIT           10
#define LITERAL_INTEGER         11

#define IDENTIFIER              50

#define _KEYWORD_GROUP          100
#define KEYWORD_RETURN          100
#define KEYWORD_MAIN            101
#define KEYWORD_PRINT           102

#define _TYPE_NAME_GROUP        200
#define TYPE_NAME               200

#define _DELIMITER_GROUP        300
#define DELIMITER_LBRACE        300
#define DELIMITER_RBRACE        301
#define DELIMITER_LPAREN        302
#define DELIMITER_RPAREN        303
#define DELIMITER_COMMA         304

#define _OPERATOR_GROUP         400
#define OPERATOR_ASSIGN         400
#define OPERATOR_PLUS           401
#define OPERATOR_MINUS          402
#define OPERATOR_TIMES          403
#define OPERATOR_DIVIDE         404

/* arbitrary limit for the token length */
#define TOKEN_TEXT_MAX_LEN      100

/* token data structure */
typedef struct _token
{
  char text[TOKEN_TEXT_MAX_LEN];
  unsigned int type,
               len,
               lineno,
               colno;
} Token;

Token *token_new();
void token_text_add_char(Token *, char);

#endif /* __TOKEN_H__ */


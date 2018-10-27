#pragma once

static const char Nul = '\0';
static const char Newline = '\n';

enum tok_type {
  TokStr,
  TokInt,
  TokColon,
  TokComma,
  TokLBrace,
  TokRBrace,
  TokLBracket,
  TokRBracket,
  TokFloat,
  TokTrue,
  TokFalse,
  TokID,
  TokRef,
  TokInvRef
};

/**
 * A key type to indicate whether the key references a normal object or an an
 * array element (using the array[X] notation).
 */
enum key_type {
  KeyNormal,
  KeyIndex
};


/**
 * Return value type for the lexer.
 *
 * LexXErr refer to errors while trying to parse tokens of type X. LexXEOF refer
 * to finding an unexpected EOF when trying to parse tokens of type X.
 *
 * LexMemErr is for memory (allocation) errors.
 *
 * LexErr is for unknown errors.
 *
 * LexEnd is for when the input has fully consumed properly. No token shall be
 * returned in that case since we reached EOF.
 *
 * LexOk is for when the returned token has successfully been parsed, but there
 * is more input to be consumed.
 *
 * IMPORTANT NOTE: everything less than LexEnd is an error!
 */
enum lex_ret {
  LexOK = 1,
  LexEnd = 0,
  LexErr = -1,
  LexMemErr = -2,
  LexRefErr = -3,
  LexRefEOF = -4,
  LexIntErr = -5,
  LexFloatErr = -6,
  LexNumErr = -7,
  LexStrErr = -8,
  LexStrEOF = -9,
  LexBoolErr = -10,
  LexBoolEOF = -11,
};

typedef enum tok_type TokType;
typedef enum key_type KeyType;
typedef enum lex_ret LexRet;

struct key {
  KeyType type;
  const char *name;
  size_t name_len;
  size_t index;
};

/**
 * A lexer token type for references. Since the lexer will also "parse" the
 * fully qualified name of the reference, this information will be stored in the
 * corresponding token's val union.
 */
struct lex_ref {
  struct key *elems;
  size_t nelems;
};

/**
 * The current state of the lexer. Used to keep track of where in the input
 * stream the lexer stopped last. Also useful for reporting errors.
 */
struct lex {
  const char *const file;
  const char *const buf;
  const char *ptr;
  size_t line;
  size_t col;
};

/**
 * The lexer token type. Contains information about each token, including the
 * position in the input stream, the originating file, etc... Note that tokens
 * of TYPE_DELIM and TYPE_ID don't get their own fields in the value union. The
 * reason for this is that they can be stored in lex_tok->tok itself and no type
 * conversion is necessary. TYPE_STR tokens still need an explicit value storage
 * because they point within in the lexeme to exclude the string double-quotes.
 *
 * Example:
 *
 * { property: "         this is a string        " }
 *             ^^                               ^^
 *             ||                               ||
 *             |-----------the string------------|
 *             ------------the token--------------
 */
struct tok {
  TokType type;
  const char *tok;
  size_t tok_len;

  const char *file;
  size_t line;
  size_t col;

  union {
    double f;                           /* TYPE_FLOAT */
    long long i;                        /* TYPE_INT */
    struct lex_ref r;                   /* TYPE_REF */
    struct {                            /* TYPE_STR */
      const char *ptr;
      size_t len;
    } s;
  } val;
};

void tok_free(const struct tok *const tok);
LexRet lex(struct lex *const lex, struct tok *const tok);

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
 * A lexer reference key type to indicate whether the key is referencing a
 * normal object or referencing an element in an array using the array[X]
 * notation.
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
 * An exception is LexMemErr is for memory (allocation) errors.
 *
 * LexErr is for unknown errors.
 *
 * LexEnd is for when the input has been fully consumed properly. No token shall
 * be returned in that case since we reached EOF.
 *
 * LexOK is for when the returned token has successfully been parsed, but there
 * is more input to be consumed.
 */
enum lex_ret {
  LexOK       = 0,
  LexEnd      = -1,
  LexErr      = -2,
  LexMemErr   = -3,
  LexRefErr   = -4,
  LexRefEOF   = -5,
  LexIntErr   = -6,
  LexFloatErr = -7,
  LexNumErr   = -8,
  LexStrErr   = -9,
  LexStrEOF   = -10,
  LexBoolErr  = -11,
  LexBoolEOF  = -12
};

typedef enum tok_type TokType;
typedef enum key_type KeyType;
typedef enum lex_ret LexRet;

struct key {
  KeyType type;
  const char *name;
  size_t namelen;
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

/**
 * The current state of the lexer. Used to keep track of where in the input
 * stream the lexer stopped last. Also keeps track of the filename, line and
 * column for reporting useful error messages.
 */
struct lex {
  const char *const file;
  const char *const buf;
  const char *ptr;
  size_t line;
  size_t col;
};

void tok_free(const struct tok *const tok);
LexRet lex(struct lex *const st, struct tok *const token);

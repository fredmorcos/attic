#ifndef __PET_PARSER__
#define __PET_PARSER__

#include <stdint.h>
#include <inttypes.h>

typedef struct {
  char      *input;
  uintmax_t  pos;
  uintmax_t  line;
  uintmax_t  col;
} Parser;

void parser_update (Parser *);

/* space consumers */
void parser_skip_spaces          (Parser *, uintmax_t *);
void parser_skip_newlines        (Parser *, uintmax_t *);
void parser_skip_spaces_newlines (Parser *, uintmax_t *);

/* generic predicate satisfaction */
void parser_satisfy (Parser *, uintmax_t *, uint8_t (*) (char));

#endif

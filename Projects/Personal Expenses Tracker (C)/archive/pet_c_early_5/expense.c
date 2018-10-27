#include "expense.h"
#include "parser.h"
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <ctype.h>

uint8_t pred_not_comma_newline (char c) {
  if (c == '\n' || c == ',')
    return 0;
  return 1;
}

uint8_t pred_comma (char c) {
  if (c == ',')
    return 1;
  return 0;
}

uint8_t expense_document_parse (char *contents, ExpenseDocument *doc) {
  Parser p;
  uintmax_t amount;

  p.input = contents;
  p.pos   = 0;
  p.line  = 1;
  p.col   = 1;

  parser_skip_spaces_newlines(&p, &amount);

 begin_persons:
  parser_satisfy(&p, &amount, pred_not_comma_newline);

  /*
   * if (amount == 0) {
   *   fprintf("Error: Could not parse
   */
  parser_satisfy(&p, &amount, pred_comma);
  puts(contents+p.pos);
  /*
   * parser_csv_line(&p, &amount); /\* persons *\/
   * parser_skip_spaces_and_newlines(&p, &amount);
   * parser_csv_line(&p, &amount); /\* shops *\/
   * parser_skip_spaces_and_newlines(&p, &amount);
   */

  return 0;
}

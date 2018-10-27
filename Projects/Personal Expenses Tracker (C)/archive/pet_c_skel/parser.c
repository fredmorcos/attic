#include "parser.h"
#include "file.h"
#include "expense.h"

char read_char (struct file_reader_t * r, enum file_reader_status_t * s) {
  *s = file_reader_next(r);
  return *r->p;
}

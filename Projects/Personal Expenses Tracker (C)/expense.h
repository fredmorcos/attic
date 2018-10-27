#ifndef EXPENSE_H
#define EXPENSE_H

#include "buffer.h"
#include "parser.h"

#include <time.h>

typedef struct tm tm_t;

typedef struct {
  tok_t amount;
  tok_t year;
  tok_t month;
  tok_t day;
  tok_t person;
  tok_t shop;
  tok_t note;
  buf_t *tags_buf;              /* token elements for each tag */

  double amount_val;
  tm_t   date_val;
} exp_t;

bool exp_print_table (buf_t *ebuf);
void exp_sort_tags (const buf_t *tags);
int exp_buf_destr (buf_t *buf);

#endif

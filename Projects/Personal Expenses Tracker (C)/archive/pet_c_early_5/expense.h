#ifndef __PET_EXPENSE__
#define __PET_EXPENSE__

#include <stdint.h>
#include <inttypes.h>

typedef struct {
  double      amount;
  uintmax_t   person;
  uintmax_t   shop;
  uint8_t     day;
  char       *note;
  char      **tags;
} Expense;

typedef struct {
  uint8_t  month;
  Expense *expenses;
} ExpenseMonth;

typedef struct {
  intmax_t      year;
  ExpenseMonth *months;
} ExpenseYear;

typedef struct {
  char        **persons;
  char        **shops;
  ExpenseYear  *years;
} ExpenseDocument;

uint8_t expense_document_parse (char *, ExpenseDocument *);

#endif

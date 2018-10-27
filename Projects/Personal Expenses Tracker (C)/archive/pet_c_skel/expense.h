#ifndef PET_EXPENSE
#define PET_EXPENSE

struct expense_t {
  double amount;
  int year;
  int month;
  int day;
  char * person;
  char * shop;
  char ** tags;
  char * note;
};

#endif

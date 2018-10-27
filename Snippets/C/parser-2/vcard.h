#ifndef VCARD_H
#define VCARD_H

#include <stdlib.h>

struct string {
  char *str;
  size_t len;
};

struct parser {
  const struct string *const buffer;

  char *p;

  size_t line;
  size_t col;
};

struct field {
  struct string  name;
  struct string *values;

  struct field *next;
  struct field *last;
};

struct vcard {
  struct field *fields;

  struct table *next;
  struct table *last;
};

int vcard_parse(const struct string *const buffer,
                struct string **vcard);

#endif

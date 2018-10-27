#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>

#define NEWLINE '\n'

typedef unsigned long ul;

void updpos(char c, ul *line, ul *col) {
  if (c == NEWLINE) {
    (*line)++;
    *col = 1;
  } else {
    (*col)++;
  }
}

int whitespace(char **p, ul *line, ul *col) {
  int res = -1;

  while (isspace(**p)) {
    res = 0;
    updpos(**p, line, col);
    (*p)++;
  }

  return res;
}

int verbatim(char **p, ul *col, char *str) {
  ul len = strlen(str);

  if (strncmp(*p, str, len) == 0) {
    *p += len;
    *col += len;
    return 0;
  } else {
    return -1;
  }
}

int verbatimc(char **p, ul *line, ul *col, char c) {
  if (**p != c) {
    return -1;
  } else {
    updpos(c, line, col);
    (*p)++;
    return 0;
  }
}

int vcard_parse(char *buf) {
  ul line = 1;
  ul col = 1;

  if ((verbatim(&buf, &col, (char *) "BEGIN:VCARD") ||
       verbatimc(&buf, &line, &col, '\n') ||
       verbatim(&buf, &col, (char *) "END:VCARD")) == 0) {
    printf("Success %lu,%lu\n", line, col);
    return 0;
  } else {
    printf("Failure %lu,%lu\n", line, col);
    return -1;
  }

  return 0;
}

int main() {
  vcard_parse((char *)
              "BEGIN:VCARD\n"
              "END:VCARD\n");

  return 0;
}

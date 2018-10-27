#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>

#define NEWLINE '\n'

typedef unsigned long ul;

int atend(const char *const *const p) {
  return **p == '\0';
}

void updatep(const char **p, ul *line, ul *col) {
  if (**p == NEWLINE) {
    if (line) {
      (*line)++;
    }

    if (col) {
      *col = 1;
    }
  } else {
    if (col) {
      (*col)++;
    }
  }

  (*p)++;
}

int whitespace(const char **p, ul *line, ul *col) {
  int res = 0;

  while (isspace(**p)) {
    res++;
    updatep(p, line, col);
  }

  return res;
}

int verbatim(const char **p, ul *line, ul *col, const char *str) {
  if (atend(&str)) {
    return 0;
  } else if (atend(p) || **p != *str) {
    return -1;
  } else {
    updatep(p, line, col);
    str++;
    return verbatim(p, line, col, str);
  }
}

int verbatimc(const char **p, ul *line, ul *col, char c) {
  if (**p != c) {
    return -1;
  } else {
    updatep(p, line, col);
    return 0;
  }
}

int until(const char **p, ul *line, ul *col, char c) {
  int len = 0;

  while (!atend(p) && **p != c) {
    updatep(p, line, col);
    len++;
  }

  return atend(p) ? -1 : len;
}

int vcard_parse(const char *buf) {
  ul line = 1;
  ul col = 1;

  const char *tmp_buf = NULL;
  int res = 0;

  whitespace(&buf, &line, &col);

  if (verbatim(&buf, &line, &col, "BEGIN:VCARD") < 0) {
    fprintf(stderr, "Error parsing BEGIN:VCARD at %lu, %lu\n", line, col);
    return -1;
  }

  if (verbatimc(&buf, &line, &col, NEWLINE) < 0) {
    fprintf(stderr, "Error parsing newline at %lu, %lu\n", line, col);
    return -1;
  }

  if (verbatim(&buf, &line, &col, "VERSION:") < 0) {
    fprintf(stderr, "Error parsing VERSION: at %lu, %lu\n", line, col);
    return -1;
  }

  tmp_buf = buf;
  res = until(&tmp_buf, &line, &col, NEWLINE);

  if (res < 0) {
    fprintf(stderr, "Premature EOF at %lu, %lu\n", line, col);
    return -1;
  } else {
    printf("Found VERSION = %.*s\n", res, buf);
    buf = tmp_buf;
  }

  if (verbatimc(&buf, &line, &col, NEWLINE) < 0) {
    fprintf(stderr, "Error parsing newline at %lu, %lu\n", line, col);
    return -1;
  }

  if (verbatim(&buf, &line, &col, "END:VCARD") < 0) {
    fprintf(stderr, "Error parsing END:VCARD at %lu, %lu\n", line, col);
    return -1;
  }

  whitespace(&buf, &line, &col);

  printf("Success finished at %lu, %lu\n", line, col);

  return 0;
}

int main() {
  vcard_parse((char *)
              "BEGIN:VCARD\n"
              "VERSION:3.0\n"
              "END:VCARD\n");

  return 0;
}

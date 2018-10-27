#include <stdio.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include "term.h"

void termsize(const int fd, int *const w, int *const h) {
  struct winsize ws;

  ioctl(fd, TIOCGWINSZ, &ws);

  if (w)
    *w = ws.ws_col;

  if (h)
    *h = ws.ws_row;
}

void termset(FILE *const f, const enum termattr *attrs) {
  static const char *const escfmt = "\x1b[%dm";

  if (isatty(fileno(f)) == 0)
    return;

  while (*attrs != TermAttrEnd)
    fprintf(f, escfmt, *attrs++);
}

void fclearline(FILE *const f) {
  if (isatty(fileno(f)))
    fprintf(f, "\r\x1b[2K");
}

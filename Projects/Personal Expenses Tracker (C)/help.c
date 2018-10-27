#include "help.h"
#include "log.h"
#include <stdio.h>

void usage (const char *fmt, ...) {
  size_t i = 0;
  char const *lines[] = {
    "PET - Personal Expense Tracker",
    "Fred Morcos <fred.morcos@gmail.com>",
    "",
    "Usage:",
    "  pet [ARGUMENTS]",
    "",
    "Arguments:",
    "  -h             Show this help and exit",
    "  -p             Print the expense results",
    "  -f {FILE | -}  Input file, `-` means stdin"
  };

  if (fmt) {
    va_list ap;

    va_start(ap, fmt);
    log_verr(fmt, ap);
    va_end(ap);

    log_err("");
  }

  for (i = 0; i < sizeof(lines) / sizeof(char *); i++) {
    fprintf(stderr, "%s\n", lines[i]);
  }
}

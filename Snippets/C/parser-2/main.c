#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "vcard.h"

int fprintf_silent(FILE *, const char *, ...);

enum cts_mode {
  cts_mode_none,
  cts_mode_check
};

int (*verbose)(FILE *, const char *, ...) = fprintf_silent;

int fprintf_silent(__attribute__((unused)) FILE *stream,
                   __attribute__((unused)) const char *format, ...) {
  return 0;
}

int help_print_global() {
  const char *const msg =
    "Usage: contacts [GLOBAL_OPTS...] [CMD] [CMD_OPTS...]\n"
    "\n"
    "Global Options:\n"
    "  -h            Show this help\n"
    "  -v            Enable verbose output\n"
    "\n"
    "Commands and Command Options:\n"
    "  check         Check a contacts list for consistency\n"
    "    -h          Show `check` command help\n"
    "    -f FILE     Check vcard FILE data for consistency\n";

  return fprintf(stderr, msg);
}

int main(int argc, char *argv[argc + 1]) {
  char *file = NULL;
  bool check_help = false;
  enum cts_mode mode = cts_mode_none;

  for (int i = 1; i < argc; i++) {
    char *arg = argv[i];

    if (strncmp("-", arg, 1) == 0) {         /* global option arg */
      if (strcmp("-v", arg) == 0) {
        verbose = fprintf;
      } else if (strcmp("-h", arg) == 0) {
        help_print_global();
        exit(0);
      } else {
        fprintf(stderr, "Unknown parameter passed: %s\n", arg);
        exit(1);
      }
    } else {                                 /* command arg */
      if (strcmp("check", arg) == 0) {
        mode = cts_mode_check;

        for (i++; i < argc; i++) {
          arg = argv[i];

          if (strcmp("-h", arg) == 0) {
            check_help = true;
          } else if (strcmp("-f", arg) == 0) {
            if (i < argc - 1) {
              i++;
            } else {
              fprintf(stderr, "Parameter not passed to -f\n");
              exit(1);
            }

            arg = file = argv[i];
          } else {
            fprintf(stderr, "Unknown parameter passed to create: %s\n", arg);
            exit(1);
          }
        }
      } else {
        fprintf(stderr, "Unknown parameter passed: %s\n", arg);
        exit(1);
      }
    }
  }

  struct string buffer = {
    .str = (char *) "      x",
    .len = 8
  };

  vcard_parse(&buffer, NULL);

  verbose(stderr, "Mode = %d\n", mode);
  verbose(stderr, "File = %s\n", file);
  verbose(stderr, "Mode (check) help = %d\n", check_help);

  return 0;
}

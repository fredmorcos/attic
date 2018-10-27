#include "args.h"
#include "help.h"
#include "log.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

bool __debug = false;
bool __quiet = false;

void args_parse (char ** filename, int argc, char ** argv) {
  if (argc == 0) {
    if (*filename == NULL) {
      log_error("No filename given");
      exit(EXIT_FAILURE);
    } else {
      return;
    }
  }

  if (strncmp(*argv, "-h", 2) == 0) {
    help_show();
    exit(EXIT_SUCCESS);
  } else if (strncmp(*argv, "-d", 2) == 0) {
    __debug = true;
    log_debug("Debugging output is now enabled");
  } else if (strncmp(*argv, "-q", 2) == 0) {
    __quiet = true;
  } else {
    if (*filename != NULL) {
      log_error("Invalid argument %s, file (%s) already given",
                *argv, *filename);
      exit(EXIT_FAILURE);
    } else {
      *filename = *argv;
    }
  }

  args_parse(filename, argc - 1, argv + 1);
}

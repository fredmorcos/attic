#include "help.h"
#include <stdio.h>

static char * help_msg[] = {
  "PET version 0.2 - Personal Expense Tracker",
  "Copyright (c) 2012-2014 Fred Morcos",
  "https://github.com/fredmorcos/pet.git",
  "",
  "Usage:",
  "  pet [ARGS...] FILENAME",
  "",
  "Arguments:",
  "  -q    Disable status messages",
  "  -d    Enable debugging messages",
  "  -h    Show this help",
  "",
  "Exit codes:",
  "  PET will exit with 0 (EXIT_SUCCESS) on success,",
  "  with 1 (EXIT_FAILURE) on errors such as file access",
  "  or memory allocation errors and will abort() with",
  "  a PANIC message on programming mistakes and invalid",
  "  or unreachable states.",
  NULL
};

void help_show () {
  int i = 0;

  while (help_msg[i] != NULL) {
    puts(help_msg[i++]);
  }
}

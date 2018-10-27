#include "help.h"
#include "cmdline.h"
#include <stdio.h>

void help_print_version (Params *p) {
  const char *const pet_name = "PET";
  const char *const pet_desc = "Personal Expense Tracker";
  const char *const pet_version = "0.2";
  const char *const pet_www = "https://github.com/fredmorcos/pet.git";
  const char *const pet_cc = "Copyright (c) 2013";
  const char *const pet_license = "2-clause BSD license";
  const char *const pet_author = "Fred Morcos";
  const char *const pet_email = "fred.morcos@gmail.com";

  fprintf(stderr, "%s (%s): %s -- Version %s\n",
          pet_name, p->progname, pet_desc, pet_version);
  fprintf(stderr, "WWW: %s\n", pet_www);
  fprintf(stderr, "%s under a %s\n", pet_cc, pet_license);
  fprintf(stderr, "%s <%s>\n", pet_author, pet_email);
}

void help_print_help (Params *p) {
  help_print_version(p);
  fprintf(stderr, "\n");
  fprintf(stderr, "Usage:\n");
  fprintf(stderr, "  %s command [parameters]\n", p->progname);
  fprintf(stderr, "\n");
  fprintf(stderr, "Commands:\n");
  fprintf(stderr, "  check <file>       Check <file> for errors\n");
}

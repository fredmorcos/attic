#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SEE_HELP " -> See `%s --help' for more information\n"

typedef enum {
  None,
  Create,
  Display
} Command;

static FILE** file_descs;
static file_descs_len =

FILE* fm_open(const char *path, const char* mode) {
  FILE* fd = NULL;

  fd = fopen(path, mode);

void show_version (const char const* prog) {
  printf("PET (%s) - Personal Expense Tracker\n"
         "Fred Morcos <fred.morcos@gmail.com>\n"
         "Copyright (c) 2012-2013 - BSD 2-Clause License\n"
         "https://github.com/fredmorcos/pet.git\n",
         prog);
}

void show_help (const char const* prog) {
  printf("Usage: %s [ARGUMENTS]\n"
         "\n"
         "Options\n"
         "  --verbose,-V      Verbose output\n"
         "  --file,-f FILE    Expenses file\n"
         "  --output,-o FILE  Expenses output file\n"
         "\n"
         "Commands\n"
         "  --help,-h         Show this help\n"
         "  --version,-v      Show version info\n"
         "  --create,-c       Create expenses file\n"
         "  --display,-d      Display expenses\n",
         prog);
}

int iscmd (char* arg) {
  if (!strcmp(arg, "--create")  || !strcmp(arg, "-c") ||
      !strcmp(arg, "--display") || !strcmp(arg, "-d"))
    return 1;
  return 0;
}

Command strtocmd(char* arg) {
  if (!strcmp(arg, "--create")  || !strcmp(arg, "-c"))
    return Create;
  else if (!strcmp(arg, "--display") || !strcmp(arg, "-d"))
    return Display;
  else
    return None;
}

char* cmdtos (Command cmd) {
  switch (cmd) {
  case None:
    return "None";
  case Create:
    return "Create";
  case Display:
    return "Display";
  default:
    return "Unknown";
  }
}

int main (int argc, char** argv) {
  char** argvc = NULL;
  char*  arg = NULL;

  int verbose = 0;
  char *file = NULL;
  char *output = NULL;
  Command cmd = None;

  if (argc < 2) {
    fprintf(stderr,
            "Error: No arguments given\n"
            SEE_HELP,
            argv[0]);
    return EXIT_FAILURE;
  }

  argvc = argv + 1;

  while (*argvc) {
    arg = *argvc;

    if (!strcmp(arg, "--verbose") || !strcmp(arg, "-v")) {
      verbose = 1;
    } else if (!strcmp(arg, "--help") || !strcmp(arg, "-h")) {
      show_version(argv[0]);
      printf("\n");
      show_help(argv[0]);
      return EXIT_SUCCESS;
    } else if (!strcmp(arg, "--version") || !strcmp(arg, "-V")) {
      show_version(argv[0]);
      return EXIT_SUCCESS;
    } else if (!strcmp(arg, "--file") || !strcmp(arg, "-f")) {
      argvc++;
      arg = *argvc;
      file = arg;
    } else if (!strcmp(arg, "--output") || !strcmp(arg, "-o")) {
      argvc++;
      arg = *argvc;
      output = arg;
    } else if (iscmd(arg)) {
      if (cmd == None) {
        cmd = strtocmd(arg);
      } else {
        fprintf(stderr,
                "Error: `%s' command already given\n"
                SEE_HELP,
                cmdtos(cmd), argv[0]);
        return EXIT_FAILURE;
      }
    } else {
      fprintf(stderr,
              "Error: Unrecognized argument `%s'\n"
              SEE_HELP,
              arg, argv[0]);
      return EXIT_FAILURE;
    }

    argvc++;
  }

  return EXIT_SUCCESS;
}

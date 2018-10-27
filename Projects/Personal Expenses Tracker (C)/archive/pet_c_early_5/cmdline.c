#include "cmdline.h"
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

static uint8_t cmdline_check_params (Params *p) {
  if (p->cmd == None) {
    fprintf(stderr, "Error: No command given.\n");
    fprintf(stderr, " -> See %s --help\n", p->progname);
    return 1;
  } else if (p->cmd != Help && p->cmd != Version && p->fn == NULL) {
    fprintf(stderr, "Error: No file given.\n");
    fprintf(stderr, " -> Command %s requires a filename\n", p->cmdname);
    return 1;
  } else {}

  return 0;
}

uint8_t cmdline_parse_args (int argc, char **argv, Params *p) {
  int i = 0;
  char *arg = NULL;

  for (i = 0; i < argc; i++) {
    arg = argv[i];
    if (strcmp(arg, "--check") == 0 || strcmp(arg, "-c") == 0) {
      if (p->cmd == None) {
        p->cmd = Check;
        p->cmdname = arg;
      } else {
        fprintf(stderr, "Error: Extraneous command argument: %s\n", arg);
        fprintf(stderr, " -> Already given: %s\n", p->cmdname);
        return 1;
      }
    } else if (strcmp(arg, "--help") == 0 || strcmp(arg, "-h") == 0) {
      p->cmd = Help;
      p->cmdname = arg;
    } else if (strcmp(arg, "--version") == 0 || strcmp(arg, "-V") == 0) {
      p->cmd = Version;
      p->cmdname = arg;
    } else if (strcmp(arg, "--verbose") == 0 || strcmp(arg, "-v") == 0) {
      p->verbose = 1;
    } else if (strcmp(arg, "--debug") == 0 || strcmp(arg, "-D") == 0) {
      p->debug = 1;
    } else if (strcmp(arg, "--bench") == 0 || strcmp(arg, "-B") == 0) {
      p->bench = 1;
    } else {
      if (strncmp(arg, "-", 1) == 0) {
        fprintf(stderr, "Error: Unknown argument: %s\n", arg);
        fprintf(stderr, " -> See %s --help\n", p->progname);
        return 1;
      } else {
        if (p->fn == NULL) {
          p->fn = arg;
        } else {
          fprintf(stderr, "Error: Extraneous file given: %s\n", arg);
          fprintf(stderr, " -> Already given file: %s\n", p->fn);
          return 1;
        }
      }
    }
  }

  return cmdline_check_params(p);
}

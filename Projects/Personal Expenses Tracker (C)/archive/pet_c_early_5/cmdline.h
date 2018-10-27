#ifndef __PET_CMDLINE__
#define __PET_CMDLINE__

#include <stdint.h>
#include <inttypes.h>

typedef enum {
  None,
  Help,
  Version,
  Check
} Cmd;

typedef struct {
  char *progname;
  Cmd cmd;
  char *cmdname;
  char *fn;
  uint8_t verbose;
  uint8_t debug;
  uint8_t bench;
} Params;

uint8_t cmdline_parse_args (int, char **, Params *);

#endif

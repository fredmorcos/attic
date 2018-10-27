#ifndef PET_CONF_H
#define PET_CONF_H

#include "log.h"
#include "buffer.h"
#include <stdbool.h>

typedef struct {
  bool do_print;
  buf_t *files;
} conf_t;

typedef enum {
  ARGPARSER_OK = 0,
  ARGPARSER_HELP,
  ARGPARSER_ERR
} argparser_ret;

argparser_ret conf_getopt (int argc, char **argv, conf_t *conf);

#endif

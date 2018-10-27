#include "conf.h"
#include "log.h"
#include "help.h"
#include "extra.h"
#include "buffer.h"
#include "file.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

argparser_ret conf_getopt (int argc, char **argv, conf_t *conf) {
  file_t finfo = { NULL, NULL, NULL };

  assert(conf != NULL);
  assert(conf->files != NULL);

  if (argc <= 1) {
    usage("Not enough arguments");
    return ARGPARSER_ERR;
  }

  for (--argc, ++argv; argc > 0; argc--, argv++) {
    if (strncmp(*argv, "-h", 2) == 0) {
      usage(NULL);
      return ARGPARSER_HELP;
    } else if (strncmp(*argv, "-p", 2) == 0) {
      conf->do_print = true;
    } else if (strncmp(*argv, "-f", 2) == 0) {
      argc--;
      argv++;

      if (argc == 0) {
        usage("Missing argument for `-f`");
        return ARGPARSER_ERR;
      }

      finfo.fn = *argv;

      if (!buf_append(conf->files, &finfo)) {
        log_syserr(errno, "Cannot append file info to buffer: %s", *argv);
        return ARGPARSER_ERR;
      }

      finfo.fn = NULL;
    } else {
      usage("Cannot recognize argument: %s", *argv);
      return ARGPARSER_ERR;
    }
  }

  return ARGPARSER_OK;
}

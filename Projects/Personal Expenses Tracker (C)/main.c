#include "log.h"
#include "buffer.h"
#include "expense.h"
#include "help.h"
#include "conf.h"
#include "file.h"
#include "extra.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <talloc.h>

int main (int argc, char **argv) {
  int i = 0;

  void *mem_ctx = NULL;
  conf_t *conf = NULL;
  buf_t *exps = NULL;

  file_t *fp = NULL;

  /*@-mustfreefresh@*/
  if (!(mem_ctx = talloc_autofree_context())) {
    log_syserr(errno, "Cannot allocate main memory context");
    return EXIT_FAILURE;
  }

  if (!(conf = talloc_zero(mem_ctx, conf_t))) {
    log_syserr(errno, "Cannot allocate configuration object");
    return EXIT_FAILURE;
  }

  /*@-mustfreeonly@*/
  if (!(conf->files = buf_init(conf, 1, sizeof(file_t)))) {
    log_syserr(errno, "Cannot allocate file list buffer");
    return EXIT_FAILURE;
  }

  talloc_set_destructor(conf->files, file_buf_destr);

  switch (conf_getopt(argc, argv, conf)) {
  case ARGPARSER_OK: break;
  case ARGPARSER_HELP: return EXIT_SUCCESS;
  case ARGPARSER_ERR: return EXIT_FAILURE;
  }

  if (!(exps = buf_init(mem_ctx, 100, sizeof(exp_t)))) {
    log_syserr(errno, "Cannot preallocate expenses buffer");
    return EXIT_FAILURE;
  }

  talloc_set_destructor(exps, exp_buf_destr);

  for (i = 0, fp = conf->files->ptr; i < conf->files->len; i++, fp++) {
    if (!file_read_all(fp) || parse_exps(fp, exps) != PARSER_OK) {
      return EXIT_FAILURE;
    }
  }

  if (conf->do_print) {
    if (!exp_print_table(exps)) {
      log_syserr(errno, "Cannot allocate buffer for printing tags");
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}

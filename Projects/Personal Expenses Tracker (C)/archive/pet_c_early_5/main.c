#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "cmdline.h"
#include "help.h"
#include "file.h"
#include "expense.h"

int main (int argc, char **argv) {
  Params params;
  char *file_contents;
  ExpenseDocument exp_doc;

  /* some (hmm..) sane defaults */
  params.progname = argv[0];
  params.cmd = None;
  params.cmdname = NULL;
  params.fn = NULL;
  params.verbose = 0;
  params.debug = 0;
  params.bench = 0;

  /* (argc - 1) and (argv + 1) is to avoid passing the program name (argv[0]) to
   * the command line argument parser
   */
  if (cmdline_parse_args(argc - 1, argv + 1, &params))
    return EXIT_FAILURE;

  switch (params.cmd) {
  case None:
    return EXIT_FAILURE;
  case Version:
    help_print_version(&params);
    return EXIT_SUCCESS;
  case Help:
    help_print_help(&params);
    return EXIT_SUCCESS;
  case Check:
    if (file_get_contents(params.fn, &file_contents))
      return EXIT_FAILURE;

    exp_doc.persons = NULL;
    exp_doc.shops   = NULL;
    exp_doc.years   = NULL;

    if (expense_document_parse(file_contents, &exp_doc)) {
      free(file_contents);
      return EXIT_FAILURE;
    }

    free(file_contents);
    break;
  }

  return EXIT_SUCCESS;
}

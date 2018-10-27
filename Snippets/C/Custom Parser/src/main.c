#include <stdlib.h>
#include "parser.h"
#include "token.h"
#include "extra.h"

int main (int argc, char **argv)
{
  FILE *input;

  if (argc < 2)
    extra_die("invalid usage, use: hllc <input-file>");

  if (!(input = fopen(argv[1], "r")))
    extra_die("could not open file for reading");

  parser_build_ast(input);

  fclose(input);

  return EXIT_SUCCESS;
}

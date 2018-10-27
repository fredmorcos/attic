#include <stdio.h>
#include <stdlib.h>
#include "args.h"
#include "file.h"
#include "parser.h"
#include <string.h>

#define FILE_CHUNK 4 * 1024

int main (int argc, char ** argv) {
  char * filename = NULL;
  FILE * file = NULL;
  struct file_reader_t reader;
  enum file_reader_status_t s = READER_OK;
  char c;
  int ret = EXIT_SUCCESS;

  args_parse(&filename, argc - 1, argv + 1);
  file = file_open(filename, "r");
  file_reader_init(&reader, file, FILE_CHUNK);

 start:
  c = read_char(&reader, &s);
  switch (s) {
  case READER_OK:
    putchar(c);
    goto start;
  case READER_EOF:
    break;
  case READER_ERR:
    ret = EXIT_FAILURE;
    break;
  }

  file_reader_destroy(&reader);
  file_close(file);

  return ret;
}

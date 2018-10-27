#ifndef PET_FILE
#define PET_FILE

#include <stdbool.h>
#include <stdio.h>
#include "buffer.h"

struct file_reader_t {
  size_t chunk;
  size_t len;
  FILE * fp;
  char * p;
  struct buffer_t * buf;
};

enum file_reader_status_t {
  READER_OK,
  READER_ERR,
  READER_EOF
};

FILE * file_open (char *, char *);
void file_close (FILE *);
void file_reader_init (struct file_reader_t *, FILE *, const size_t);
void file_reader_destroy (struct file_reader_t *);
enum file_reader_status_t file_reader_next (struct file_reader_t *);

#endif

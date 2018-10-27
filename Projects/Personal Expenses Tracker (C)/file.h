#ifndef PET_FILE_H
#define PET_FILE_H

#include "buffer.h"
#include <stdio.h>
#include <stdbool.h>

typedef struct {
  FILE *fd;
  char *fn;
  buf_t *data;
} file_t;

bool file_read_all (file_t *f);
bool file_read_stdin (file_t *f);
int file_buf_destr (buf_t *buf);

#endif

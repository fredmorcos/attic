#include "file.h"
#include "log.h"
#include "buffer.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <string.h>

FILE * file_open (char * filename, char * mode) {
  FILE * fp = NULL;

  if ((fp = fopen(filename, mode)) == NULL) {
    int tmperr = errno;
    log_error("Cannot open file %s", filename);
    log_syserr(tmperr);
    exit(EXIT_FAILURE);
  }

  return fp;
}

void file_close (FILE * fp) {
  int ret = fclose(fp);

  if (ret == 0) {
    return;
  } else if (ret == EOF) {
    int tmperr = errno;
    log_error("Cannot close file");
    log_syserr(tmperr);
    exit(EXIT_FAILURE);
  } else {
    log_panic("Invalid program state, fclose returned %d", ret);
    abort();
  }
}

void file_reader_init (struct file_reader_t * r, FILE * fp, const size_t c) {
  r->buf = buffer_new(sizeof(char), c);

  if (r->buf == NULL) {
    log_error("Cannot allocate memory for file buffer");
    exit(EXIT_FAILURE);
  }

  r->p = r->buf->buf;
  r->len = 0;
  r->chunk = c;
  r->fp = fp;
}

void file_reader_destroy (struct file_reader_t * r) {
  buffer_destroy(r->buf);
}

enum file_reader_status_t file_reader_next (struct file_reader_t * r) {
  if (r->p == (char *) r->buf->buf + r->len - (r->len == 0 ? 0 : 1)) {
    r->len = fread(r->buf->buf, 1, r->chunk, r->fp);

    if (r->len == 0) {
      if (ferror(r->fp) != 0) {
        int tmperr = errno;
        log_error("Error reading from file");
        log_syserr(tmperr);
        return READER_ERR;
      } else if (feof(r->fp) != 0) {
        return READER_EOF;
      } else {
        log_error("Nothing was read from file");
        return READER_ERR;
      }
    } else {
      r->p = r->buf->buf;
    }
  } else {
    r->p++;
  }

  return READER_OK;
}

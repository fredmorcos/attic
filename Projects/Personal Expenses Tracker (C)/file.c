#include "file.h"
#include "buffer.h"
#include "log.h"
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>
#include <sys/stat.h>
#include <talloc.h>

bool file_read_stdin (file_t *f) {
  static const size_t tsize = 1024;

  int c;

  assert(f != NULL);
  assert(f->fd == stdin);
  assert(f->data == NULL);

  if (!(f->data = buf_init(NULL, tsize, sizeof(char)))) {
    log_syserr(errno, "Cannot allocate %lu bytes for stdin", tsize);
    f->fd = NULL;
    return false;
  }

  while ((c = getchar()) != EOF) {
    if (!buf_append(f->data, &c)) {
      log_syserr(errno, "Cannot append char to stdin buffer");
      f->fd = NULL;
      TALLOC_FREE(f->data);
      return false;
    }
  }

  return true;
}

bool file_read_all (file_t *f) {
  bool ret = true;
  size_t tsize = 0;
  size_t nread = 0;
  struct stat st;

  assert(f != NULL);
  assert(f->fn != NULL);
  assert(f->fd == NULL);
  assert(f->data == NULL);

  if (strncmp(f->fn, "-", 1) == 0) {
    f->fd = stdin;
    return file_read_stdin(f);
  }

  if (stat(f->fn, &st)) {
    log_syserr(errno, "Cannot stat %s", f->fn);
    return false;
  }

  tsize = st.st_size;

  if (!(f->fd = fopen(f->fn, "r"))) {
    log_syserr(errno, "Cannot open %s", f->fn);
    return false;
  }

  if (!(f->data = buf_init(NULL, tsize, sizeof(char)))) {
    log_syserr(errno, "Cannot allocate %lu bytes for %s", tsize, f->fn);
    ret = false;
    goto quit_close_fd;
  }

  if ((nread = fread(f->data->ptr, 1, tsize, f->fd)) < (size_t) tsize) {
    if (ferror(f->fd)) {
      log_syserr(errno, "Cannot read %s", f->fn);
    }

    log_err("Only read %lu bytes, expected %lu", nread, tsize);

    ret = false;
    goto quit_free_data;
  }

  if (fclose(f->fd) == EOF) {
    log_syserr(errno, "Cannot close %s", f->fn);
  }

  f->fd = NULL;
  f->data->len = tsize;

  return true;

 quit_free_data:
  TALLOC_FREE(f->data);

 quit_close_fd:
  if (fclose(f->fd) == EOF) {
    log_syserr(errno, "Cannot close %s", f->fn);
  }

  f->fd = NULL;

  return ret;
}

int file_buf_destr (buf_t *buf) {
  file_t *f = NULL;

  assert(buf);
  assert(buf->ptr);

  f = buf->ptr;

  for (int i = 0; i < buf->len; i++, f++) {
    if (f->fd && fclose(f->fd) == EOF) {
      if (f->fd == stdin) {
        log_syserr(errno, "Cannot close stdin");
      } else if (f->fn) {
        log_syserr(errno, "Cannot close file: %s", f->fn);
      } else {
        log_syserr(errno, "Cannot close file");
      }
    }

    if (f->data) {
      TALLOC_FREE(f->data);
    }
  }

  return 0;
}

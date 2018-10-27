#define _POSIX_C_SOURCE 1

#include "file.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <sys/stat.h>

#include <talloc.h>

char *
file_read_all(const void *const context, const char *const filename) {
  assert(filename != NULL);

  char *buffer = NULL;
  FILE *fp = NULL;
  size_t len = 0;
  struct stat st;

  void *memctx = talloc_autofree_context();

  if ((fp = fopen(filename, "r")) == NULL) {
    perror(filename);
    return NULL;
  }

  if (fstat(fileno(fp), &st) == -1) {
    perror(filename);
    goto finish;
  }

  if (st.st_size < 0) {
    fprintf(stderr, "%s: Strange file size %lu\n", filename, st.st_size);
    goto finish;
  } else if (st.st_size == 0) {
    fprintf(stderr, "%s: Empty file\n", filename);
    goto finish;
  }

  if ((buffer = talloc_array(memctx, char, st.st_size + 1)) == NULL) {
    perror(filename);
    goto finish;
  }

  if ((len = fread(buffer, sizeof(char), (size_t) st.st_size, fp)) <
      (size_t) st.st_size) {
    if (ferror(fp) != 0) {
      fprintf(stderr, "%s: Error reading file\n", filename);
      free(buffer);
      buffer = NULL;
      goto finish;
    }
  }

  buffer[len] = '\0';
  talloc_reparent(memctx, context, buffer);

 finish:
  if (fclose(fp) == EOF) {
    perror(filename);
  }

  return buffer;
}

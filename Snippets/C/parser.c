#include <assert.h>
#include <err.h>
#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <sysexits.h>

char *read_file(char *filename);

/*
 * struct state {
 *   char *buffer;
 *   char *pointer;
 *
 *   size_t length;
 *   size_t remain_length;
 * };
 *
 * int parse_expression(struct state *state) {
 * }
 */

char *read_file(char *filename) {
  FILE *file;

  char *new_buffer;

  char *buffer = NULL;
  size_t buffer_length = 0;
  size_t buffer_capacity = 0;

  const size_t chunk_len = 2048;

  file = fopen(filename, "r");

  if (!file)
    return NULL;

  while (1) {
    size_t fread_length;

    if (buffer_capacity - buffer_length < chunk_len) {
      new_buffer = realloc(buffer, buffer_capacity + chunk_len);

      if (!new_buffer) {
        int __errno = errno;
        (void) fclose(file);
        free(buffer);
        errno = __errno;
        return NULL;
      }

      buffer = new_buffer;
      buffer_capacity += chunk_len;
    }

    fread_length = fread(buffer + buffer_length, 1,
                         buffer_capacity - buffer_length, file);

    if (fread_length < buffer_capacity - buffer_length) {
      if (ferror(file)) {
        int __errno = errno;
        (void) fclose(file);
        free(buffer);
        errno = __errno;
        return NULL;
      }

      if (feof(file))
        break;
    }

    buffer_length += fread_length;
  }

  (void) fclose(file);

  return buffer;
}

int main (int argc, char **argv) {
  char *filename;
  char *buffer;

  if (argc < 2 || argc > 3)
    err(EX_USAGE, "Usage: parser FILE");

  filename = argv[1];

  buffer = read_file(filename);

  if (!buffer)
    err(EX_OSERR, "Could not read %s", filename);

  printf("%s", buffer);

  return 0;
}

#include "file.h"
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdint.h>
#include <inttypes.h>

static uint8_t file_get_size (const char *filename, int64_t *size) {
  struct stat st;

  if (stat(filename, &st) == 0) {
    *size = st.st_size;
    return 0;
  }

  fprintf(stderr, "Error: %s\n", strerror(errno));
  fprintf(stderr, " -> Failed to read file size.\n");
  fprintf(stderr, " -> File: %s\n", filename);
  return 1;
}

uint8_t file_get_contents (const char *filename, char **contents) {
  int64_t  size         = 0;
  size_t   size_to_read = 0;
  size_t   read_size    = 0;
  FILE    *f            = NULL;

  if (file_get_size(filename, &size))
    return 1;

  size_to_read = sizeof(char) * size;

  /* allocate one byte larger than needed for NUL termination of the contents
   * string: sizeof(char) * (size + 1)
   */
  *contents = (char *) malloc(size_to_read + sizeof(char));

  if (*contents == NULL) {
    fprintf(stderr, "Error: %s\n", strerror(errno));
    fprintf(stderr, " -> Memory allocation failed for file contents.\n");
    fprintf(stderr, " -> File: %s\n", filename);
    return 1;
  }

  /* set NUL termination of the content string in the last element */
  (*contents)[size] = '\0';

  f = fopen(filename, "r");

  if (f == NULL) {
    fprintf(stderr, "Error: %s\n", strerror(errno));
    fprintf(stderr, " -> Failed to open file.\n");
    fprintf(stderr, " -> File: %s\n", filename);

    free(*contents);
    *contents = NULL;

    return 1;
  }

  read_size = fread(*contents, sizeof(char), size, f);

  if (read_size < size_to_read) {
    fprintf(stderr, "Error: Data size read from file less than expected.\n");
    fprintf(stderr, " -> Expected: %ld\n", (int64_t) size_to_read);
    fprintf(stderr, " -> Actual: %ld\n", (int64_t) read_size);
    fprintf(stderr, " -> File: %s\n", filename);

    if (ferror(f)) {
      fprintf(stderr, "Error: Unknown error when reading from file.\n");
      fprintf(stderr, " -> File: %s\n", filename);
      clearerr(f);
    }

    if (feof(f)) {
      fprintf(stderr, "Error: Premature end-of-file.\n");
      fprintf(stderr, " -> File: %s\n", filename);
    }

    if (fclose(f) == EOF) {
      fprintf(stderr, "Error: %s\n", strerror(errno));
      fprintf(stderr, " -> Failed to close file.\n");
      fprintf(stderr, " -> File: %s\n", filename);
    }

    free(*contents);
    *contents = NULL;

    return 1;
  }

  if (fclose(f) == EOF) {
    fprintf(stderr, "Error: %s\n", strerror(errno));
    fprintf(stderr, " -> Failed to close file.\n");
    fprintf(stderr, " -> File: %s\n", filename);

    free(*contents);
    *contents = NULL;

    return 1;
  }

  return 0;
}

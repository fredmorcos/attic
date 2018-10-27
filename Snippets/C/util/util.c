#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

#include "util.h"

FILE * open_file(const char *const file_name, const char *const mode)
{
  FILE *stream;

#if defined(_MSC_VER)
  errno_t error;
  error = fopen_s(&stream, file_name, mode);
  if (error != 0) {
    return NULL;
  }
#else
  stream = fopen(file_name, mode);
#endif

  return stream;
}

char *strfmt(const char *format, ...) {

  va_list args;
  char *buffer;
  size_t needed, actuallyUsed;

  va_start(args, format);

  buffer = NULL;

  needed = vsnprintf(NULL, 0, format, args);

  if (needed > 0) {

    buffer = malloc(needed + 1);

    if (buffer != NULL) {

      actuallyUsed = vsnprintf(buffer, needed + 1, format, args);

      if (actuallyUsed != needed) {
        free(buffer);
        buffer = NULL;
      }
    }
  }

  va_end(args);

  return buffer;
}

char *read_file_str(const char *const file_name) {

  FILE *fp;
  long size;
  char *buffer;

  fp = open_file(file_name, "rb");
  if (!fp) {
    return NULL;
  }

  fseek(fp, 0L, SEEK_END);
  size = ftell(fp);

  if (size == -1) {
    fclose(fp);
    return NULL;
  }

  rewind(fp);

  buffer = calloc(1, size + 1);
  if (buffer == NULL) {
    fclose(fp);
    return NULL;
  }

  if (fread(buffer, size, 1, fp) != 1) {
    fclose(fp);
    free(buffer);
    return NULL;
  }

  fclose(fp);
  return buffer;
}

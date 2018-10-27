#include <assert.h>
#include <errno.h>
#include <err.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sysexits.h>

#define CD_VERSION "0.1"

int main (int argc, char **argv) {
  if (argc < 2)
    errx(EX_USAGE, "Error: No arguments given");
  else if (argc > 2)
    errx(EX_USAGE, "Error: Too many arguments given");
  else if (!strncmp(argv[1], "-h", 2))
    errx(EX_OK, "Usage: confdump [-h] FILE");
  else if (!strncmp(argv[1], "-V", 2))
    errx(EX_OK, "version %s", CD_VERSION);

  FILE *const fp = fopen(argv[1], "r");

  if (!fp)
    err(EX_OSERR, "Cannot open config file %s", argv[1]);

  int c;

  while ((c = fgetc(fp)) != EOF) {
  }

 finish_parse:

  if (ferror(fp) != 0) {
    warn("Error reading from config file %s", argv[1]);
    if (fclose(fp) == EOF)
      err(EX_OSERR, "Could not close config file %s", argv[1]);
    return EX_OSERR;
  }

  if (!feof(fp)) {
    warnx("Unknown error reading from config file %s", argv[1]);
    if (fclose(fp) == EOF)
      err(EX_OSERR, "Could not close config file %s", argv[1]);
    return EX_OSERR;
  }

  if (fclose(fp) == EOF)
    err(EX_OSERR, "Could not close config file %s", argv[1]);

  return EX_OK;
}

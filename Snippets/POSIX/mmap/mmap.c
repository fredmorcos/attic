#include <assert.h>
#include <err.h>
#include <errno.h>
#include <sysexits.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <limits.h>
#include <unistd.h>
#include <math.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

char *map_file(const char *const fn, struct stat *const st);

char *
map_file(const char *const fn, struct stat *const st)
{
  assert(fn != NULL);

  const int fd = open(fn, O_RDONLY);

  if (fd == -1) {
    warn("error: cannot open %s", fn);
    return NULL;
  }

  if (fstat(fd, st) == -1) {
    warn("error: cannot stat %s (fd %d)", fn, fd);
    (void) close(fd);
    return NULL;
  }

  char *const map = mmap(NULL, st->st_size, PROT_READ, MAP_PRIVATE, fd, 0);

  if (map == NULL) {
    warn("error: cannot map %s (fd %d)", fn, fd);
    (void) close(fd);
    return NULL;
  }

  (void) close(fd);
  return map;
}

int
main(int argc, char *argv[])
{
  if (argc < 2)
    errx(EX_USAGE, "error: no arguments");
  else if (argc > 2)
    errx(EX_USAGE, "error: too many arguments");

  struct stat main_file_st;
  char *const main_file = map_file(argv[1], &main_file_st);

  if (main_file == NULL) {
    warnx("error: cannot read main file %s", argv[1]);
    return EX_OSERR;
  }

  for (long i = 0; i < main_file_st.st_size; i++)
    putc(main_file[i], stdout);
  fflush(stdout);

  (void) munmap(main_file, main_file_st.st_size);
  return EX_OK;
}

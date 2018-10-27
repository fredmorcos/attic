#include <assert.h>
#include <err.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

int main(int argc, char *argv[]) {
  struct stat st1;
  struct stat st2;
  struct stat *sta = &st1;
  struct stat *stb = &st2;

  if (argc < 3)
    errx(2, "too few arguments");

  if (stat(argv[1], &st1) == -1)
    err(3, "cannot stat %s", argv[1]);

  if (sta->st_nlink != (unsigned long) argc - 1)
    return 1;

  for (int i = 2; i < argc - 1; i++) {
    if (stat(argv[i], stb) == -1)
      err(3, "cannot stat %s", argv[i + 1]);

    if (stb->st_nlink != (unsigned long) argc - 1 ||
        sta->st_dev   != stb->st_dev  ||
        sta->st_ino   != stb->st_ino  ||
        sta->st_size  != stb->st_size)
      return 1;

    sta = stb;
  }

  return 0;
}

/*
 * gcc -Wall -Wextra -pedantic -std=c99 mmap_test.c -o mmap_test
 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>

int main(int argc, char *argv[]) {
  struct stat st;
  char *map;
  int fd;

  if (argc < 2) {
    fprintf(stderr, "Error: no filename given\n");
    return 1;
  } else if (argc > 2) {
    fprintf(stderr, "Error: too many arguments given\n");
    return 1;
  }

  fd = open(argv[1], O_RDONLY);

  if (fd == -1) {
    perror("open()");
    return 1;
  }

  fstat(fd, &st);

  map = mmap(NULL, st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);

  if (map == NULL) {
    perror("mmap()");
    close(fd);
    return 1;
  }

  for (int i = 0; i < st.st_size; i++) {
    putc(map[i], stdout);
  }

  fflush(stdout);
  munmap(map, st.st_size);
  close(fd);
  return 0;
}

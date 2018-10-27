#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>

int main(int argc, char **argv) {
  FILE *fp = NULL;
  char *buf = NULL;

  struct stat st;

  if (argc < 2) {
    fprintf(stderr, "Error: no file given\n");
    exit(EXIT_FAILURE);
  }

  if (stat(argv[1], &st) == -1) {
    fprintf(stderr, "Error: cannot stat `%s': %s\n",
            argv[1], strerror(errno));
    exit(EXIT_FAILURE);
  }

  if ((buf = malloc((size_t) st.st_size)) == NULL) {
    fprintf(stderr, "Error: cannot allocate memory for `%s': %s\n",
            argv[1], strerror(errno));
    exit(EXIT_FAILURE);
  }

  if ((fp = fopen(argv[1], "r")) == NULL) {
    fprintf(stderr, "Error: cannot open `%s': %s\n",
            argv[1], strerror(errno));
    exit(EXIT_FAILURE);
  }

  if (fread(buf, 1, (size_t) st.st_size, fp) < (size_t) st.st_size) {
  }

  if (fclose(fp) == EOF) {
    fprintf(stderr, "Warning: cannot close `%s': %s\n",
            argv[1], strerror(errno));
  }

  free(buf);

  return EXIT_SUCCESS;
}

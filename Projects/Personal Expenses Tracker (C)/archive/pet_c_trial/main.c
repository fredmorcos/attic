#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <err.h>
#include <ctype.h>

#define VERSION 1               /* pet version */
#define BLOCKSIZE (1024 * 1024) /* about 1MB buffer block */

int consume_cmd(int, char **);
int consume_args_show(int, char **);
int parse_input_file(char *);
void usage();
int fread_all(FILE *, char *, int *);

extern char *__progname;

int fread_all(FILE *f, char *data, int *size) {
  int bufindex = 0;
  int count = 0;
  int malloc_size = sizeof(char) * BLOCKSIZE;

  *size = 0;
  data = (char *) malloc(malloc_size);

  if (!data) {
    /* TODO */
    fprintf(stderr, "%s: Cannot allocate space for %s",
            __progname, "<filename>");
    return 1;
  }

  while ((count = fread(&data[bufindex], 1, BLOCKSIZE, f)) == BLOCKSIZE) {
    *size += BLOCKSIZE;
    malloc_size += sizeof(char) * BLOCKSIZE;
    bufindex += BLOCKSIZE;

    data = (char *) realloc(data, malloc_size);

    if (!data) {
      free(data);
      data = NULL;
      /* TODO */
      fprintf(stderr, "%s: Cannot allocate space for %s",
              __progname, "<filename>");
      return 1;
    }
  }

  if (count) {
    if (feof(f)) {
      *size += count;
      data[*size] = '\0';
      return 0;
    } else {
      fprintf(stderr, "%s: Not sure why this was reached", __progname);
      return 1;
    }
  } else {
    if (ferror(f)) {
      fprintf(stderr, "%s: Error reading from file %s", __progname, "<filename>");
      return 1;
    } else {
      fprintf(stderr, "%s: Not sure why this was reached", __progname);
      return 1;
    }
  }
  return 0;
}

int parse_input_file(char *str)
{
  /*
   * char **input_saveptr;
   * char *tok;
   *
   * if (tok = strtok_r(str, "\n", input_saveptr)) {
   *   parse_expense(
   *                 while (strtok_r(str, "\n", input_saveptr))
   *                   }
   * }
   */
  return 0;
}

int main(int argc, char **argv)
{
  if (argc < 2 || strcmp("help", argv[1]) == 0) {
    usage();
    return EXIT_FAILURE;
  } else {
    if (consume_cmd(argc - 1, argv + 1) != 0)
      return EXIT_FAILURE;
    else
      return EXIT_SUCCESS;
  }
}

int consume_cmd(int argc, char **argv)
{
  if (argc > 0) {
    if (strcmp(argv[0], "show") == 0) {
      return consume_args_show(argc - 1, argv + 1);
    } else {
      fprintf(stderr, "%s: Unknown command: %s\n", __progname, argv[0]);
      return 1;
    }
  } else {
    fprintf(stderr, "%s: No command given\n", __progname);
    return 1;
  }
}

int consume_args_show(int argc, char **argv)
{
  FILE *f = NULL;

  if (argc > 0) {
    f = fopen(argv[0], "r");

    if (f == NULL) {
      warn(argv[0]);
      return 1;
    } else {
      /*
       * if (parse_input_file(f) != 0)
       *   return 1;
       */
      char *data = NULL;
      int size;

      if (fread_all(f, data, &size) != 0) {
        fclose(f);
        return 1;
      }

      printf("%lu\n", sizeof(data));
      printf("%s\n", data);
      free(data);

      if (fclose(f) != 0) {
        warn(argv[0]);
        return 1;
      } else {
        return 0;
      }
    }
  } else {
    fprintf(stderr, "%s: No arguments to `show'\n", __progname);
    return 1;
  }
}

void usage()
{
  printf("Personal Expense Tracker version %d\n", VERSION);
  printf("Fred Morcos  2012-2013\n");
  printf("https://github.com/fredmorcos/pet.git\n");
  printf("\n");
  printf("usage: pet [COMMAND] [ARGS]\n");
  printf("\n");
  printf("COMMANDS\n");
  printf("  help       show this help\n");
  printf("  show FILE  show expenses from FILE\n");
}

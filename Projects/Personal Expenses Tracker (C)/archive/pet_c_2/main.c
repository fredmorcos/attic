#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <err.h>
#include <ctype.h>

#define VERSION 1               /* pet version */

int consume_cmd(int, char **);
int consume_args_show(int, char **);
int parse_input_file(FILE *);
int parse_expense(FILE *);
int parse_read_space(FILE *);
int parse_read_char(FILE *, char);
char *parse_read_int(FILE *);
void usage();

extern char *__progname;

int parse_read_space(FILE *f)
{
  char c = 0;

  c = fgetc(f);

  if (isspace(c)) {
    parse_read_space(f);
    return 0;
  }

  ungetc(c, f);
  return 1;
}

int parse_read_char(FILE *f, char x)
{
  char c = 0;

  c = fgetc(f);

  if (c == x)
    return 0;

  ungetc(c, f);
  return 1;
}

char *parse_read_int(FILE *f)
{
  return NULL;
}

int parse_expense(FILE *f)
{
  int amount_dec = 0;
  int amount_frac = 0;
  int date_year = 0;
  int date_month = 0;
  int date_day = 0;

  parse_read_space(f);
  parse_read_int(f);
  parse_read_char(f, '.');
  parse_read_int(f);
  parse_read_space(f);
  parse_read_int(f);
  parse_read_char(f, '-');
  parse_read_int(f);
  parse_read_char(f, '-');
  parse_read_int(f);
  parse_read_space(f);

  return 0;
}

int parse_input_file(FILE *f)
{
  return parse_expense(f);
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
      if (parse_input_file(f) == 0) {
      }

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

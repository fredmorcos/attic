#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define VERSION "0.2"
#define EOS '\0'

typedef struct _command {
  char *cmd;
  void (*func) ();
} command;

typedef struct _global_opts {
  int verbose;
} global_opts;

void show_help ();
void show_version ();
void temp_dummy ();
int is_command (char *, command *);
int stricmp (char *, char *);

command cmdlist[] = {
  { .cmd = "add",      .func = temp_dummy },
  { .cmd = "show",     .func = temp_dummy },
  { .cmd = "help",     .func = temp_dummy },
  { .cmd = "optimize", .func = temp_dummy },
  { .cmd = "version",  .func = temp_dummy },
  { .cmd = NULL,       .func = NULL }
};

int stricmp (char *a, char *b)
{
  while (*a != EOS && *b != EOS) {
    if (tolower(*a) != tolower(*b))
      return 1;
    a++;
    b++;
  }

  if (*a != EOS || *b != EOS)
    return 1;
  return 0;
}

int is_command (char *cmd, command *list)
{
  command *c = list;

  while (c->cmd != NULL) {
    if (!stricmp(cmd, c->cmd))
      return 1;
    c++;
  }

  return 0;
}

void temp_dummy ()
{
  return;
}

void show_help ()
{
  puts("usage");
  puts("  pet [global-options] <command> [command-options] [FILE ...]");
  puts("");
  puts("commands");
  puts("  add             Add an expense");
  puts("  show            Show expenses");
  puts("  help            Show this help");
  puts("  optimize        Optimize expenses file");
  puts("  version         Show version info");
  puts("");
  puts("global options");
  puts("  --verbose,-v    Be verbose with output");
  puts("");
  puts("options for show");
  puts("  --extended/-e   Print extended dates");
}

void show_version ()
{
  puts("PET -- The Personal Expense Tracker -- Version " VERSION);
  puts("Copyright 2012-2013 Fred Morcos <fred.morcos@gmail.com>");
  puts("http://github.com/fredmorcos/pet.git");
  puts("Licensed under the GPLv3");
}

int main (int argc, char **argv)
{
  int i;
  global_opts gopts = { .verbose = 0 };

  if (argc <= 1) {              /* no arguments given */
    show_help();
    exit(EXIT_FAILURE);
  }

  /* try to read global options until we encounter a command */
  for (i = 1; i < argc; i++) {
    if (is_command(argv[i], cmdlist)) {
      puts("we have a command!");
    }
  }

  return 0;
}
